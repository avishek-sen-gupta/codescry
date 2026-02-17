/**
 * Stub script that intercepts tree-sitter's grammar() call
 * and outputs the rule names as a JSON array.
 *
 * Also stubs the tree-sitter DSL functions (choice, seq, token, etc.)
 * so that grammars using them at module scope don't crash.
 *
 * Usage: node extract_rules.js <path/to/grammar.js>
 */

// -- DSL function stubs (return dummy objects) ----------------------------
const DUMMY = {};
const noop = () => DUMMY;

global.alias = noop;
global.blank = noop;
global.choice = noop;
global.field = noop;
global.optional = noop;
global.repeat = noop;
global.repeat1 = noop;
global.seq = noop;
global.sym = noop;

global.token = Object.assign(noop, { immediate: noop });
global.prec = Object.assign(noop, {
  left: noop,
  right: noop,
  dynamic: noop,
});

// -- grammar() stub -------------------------------------------------------
global.grammar = function (baseGrammarOrOptions, maybeOptions) {
  const options = maybeOptions || baseGrammarOrOptions;
  process.stdout.write(JSON.stringify(Object.keys(options.rules || {})));
  return options;
};

require(require("path").resolve(process.argv[2]));
