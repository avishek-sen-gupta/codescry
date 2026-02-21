"""Lightweight TF-IDF + Logistic Regression classifier for integration signals.

Predicts one of three classes (DEFINITE_INWARD, DEFINITE_OUTWARD, NOT_DEFINITE)
from a single signal_line_content string with no API dependency at inference time.
"""

from __future__ import annotations

from pathlib import Path

import joblib
from sklearn.linear_model import LogisticRegression
from sklearn.pipeline import Pipeline, FeatureUnion
from sklearn.feature_extraction.text import TfidfVectorizer

from repo_surveyor.training.types import TRAINING_LABELS, TrainingExample, TrainingLabel


class _ModelConfig:
    """Hyperparameters for the TF-IDF + LR pipeline."""

    WORD_NGRAM_MIN = 1
    WORD_NGRAM_MAX = 2
    CHAR_NGRAM_MIN = 3
    CHAR_NGRAM_MAX = 6
    LR_MAX_ITER = 1000
    LR_C = 1.0
    LR_CLASS_WEIGHT = "balanced"


def _build_pipeline() -> Pipeline:
    """Construct the sklearn pipeline with FeatureUnion of TF-IDF vectorisers."""
    word_tfidf = TfidfVectorizer(
        analyzer="word",
        ngram_range=(_ModelConfig.WORD_NGRAM_MIN, _ModelConfig.WORD_NGRAM_MAX),
    )
    char_tfidf = TfidfVectorizer(
        analyzer="char_wb",
        ngram_range=(_ModelConfig.CHAR_NGRAM_MIN, _ModelConfig.CHAR_NGRAM_MAX),
    )
    features = FeatureUnion(
        [
            ("word_tfidf", word_tfidf),
            ("char_tfidf", char_tfidf),
        ]
    )
    classifier = LogisticRegression(
        max_iter=_ModelConfig.LR_MAX_ITER,
        C=_ModelConfig.LR_C,
        class_weight=_ModelConfig.LR_CLASS_WEIGHT,
    )
    return Pipeline([("features", features), ("clf", classifier)])


class SignalClassifier:
    """TF-IDF + Logistic Regression classifier for integration signal lines.

    Predicts DEFINITE_INWARD, DEFINITE_OUTWARD, or NOT_DEFINITE from a
    single signal_line_content string.
    """

    def __init__(self, pipeline: Pipeline, label_names: list[str]) -> None:
        self._pipeline = pipeline
        self._label_names = label_names

    @classmethod
    def train(
        cls,
        train_examples: list[TrainingExample],
        val_examples: list[TrainingExample],
    ) -> "SignalClassifier":
        """Fit a new classifier on train_examples and evaluate on val_examples.

        Args:
            train_examples: Labelled examples for fitting.
            val_examples: Labelled examples for validation reporting.

        Returns:
            A trained SignalClassifier instance.
        """
        from sklearn.metrics import classification_report, accuracy_score

        train_texts = [ex.signal_line_content for ex in train_examples]
        train_labels = [ex.label for ex in train_examples]

        pipeline = _build_pipeline()
        pipeline.fit(train_texts, train_labels)

        label_names = sorted({ex.label for ex in train_examples})
        classifier = cls(pipeline, label_names)

        val_texts = [ex.signal_line_content for ex in val_examples]
        val_true = [ex.label for ex in val_examples]
        val_pred = pipeline.predict(val_texts)

        accuracy = accuracy_score(val_true, val_pred)
        report = classification_report(val_true, val_pred)

        print(f"\nVal accuracy: {accuracy:.4f}")
        print(report)

        return classifier

    def predict(self, signal_line: str) -> TrainingLabel:
        """Predict the label for a single signal line.

        Args:
            signal_line: The signal_line_content string to classify.

        Returns:
            The predicted TrainingLabel.
        """
        raw = self._pipeline.predict([signal_line])[0]
        return TrainingLabel(raw)

    def predict_proba(self, signal_line: str) -> dict[TrainingLabel, float]:
        """Predict class probabilities for a single signal line.

        Args:
            signal_line: The signal_line_content string to classify.

        Returns:
            Dictionary mapping each TrainingLabel to its predicted probability.
        """
        proba = self._pipeline.predict_proba([signal_line])[0]
        classes = self._pipeline.classes_
        return {TrainingLabel(cls): float(p) for cls, p in zip(classes, proba)}

    def save(self, path: Path) -> None:
        """Serialise the classifier to disk using joblib.

        Args:
            path: File path for the serialised model.
        """
        path.parent.mkdir(parents=True, exist_ok=True)
        joblib.dump(
            {"pipeline": self._pipeline, "label_names": self._label_names}, path
        )

    @classmethod
    def load(cls, path: Path) -> "SignalClassifier":
        """Deserialise a classifier from disk.

        Args:
            path: File path of the serialised model.

        Returns:
            A loaded SignalClassifier instance.
        """
        payload = joblib.load(path)
        return cls(payload["pipeline"], payload["label_names"])

    @property
    def model_id(self) -> str:
        """Human-readable descriptor for this model."""
        return (
            "signal_classifier:tfidf_word_char+logistic_regression"
            f":C={_ModelConfig.LR_C}"
            f":class_weight={_ModelConfig.LR_CLASS_WEIGHT}"
        )


class NullSignalClassifier(SignalClassifier):
    """Null object SignalClassifier that always predicts NOT_DEFINITE.

    Used as the default when no trained classifier is available, so the
    pipeline can run without a model file.
    """

    def __init__(self) -> None:
        pass

    def predict(self, signal_line: str) -> TrainingLabel:
        return TrainingLabel.NOT_DEFINITE

    def predict_proba(self, signal_line: str) -> dict[TrainingLabel, float]:
        return {label: 0.0 for label in TRAINING_LABELS}

    @property
    def model_id(self) -> str:
        return "signal_classifier:null"
