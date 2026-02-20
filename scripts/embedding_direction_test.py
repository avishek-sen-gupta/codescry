"""Test whether nomic-embed-code can classify integration signal DIRECTION.

The first experiment proved embeddings distinguish integration TYPE (100% accuracy).
The second proved non-I/O code scores below the classification threshold.
This experiment tests the finer-grained question: can embeddings distinguish
INWARD (exposing an endpoint/service) from OUTWARD (consuming an external system)
within the same integration type?

Uses directional descriptions (type × direction) and code fragments tagged
with (language, type, direction). Reports:
  1. Full classification accuracy (type + direction jointly)
  2. Direction-only accuracy (given correct type, is direction correct?)
"""

import math
import os
import sys

import requests

ENDPOINT = os.environ["HUGGING_FACE_URL"]
TOKEN = os.environ["HUGGING_FACE_API_TOKEN"]

# Directional descriptions: (integration_type, direction) → natural language
DIRECTIONAL_DESCRIPTIONS = {
    ("http_rest", "inward"): (
        "Exposes an HTTP or REST endpoint that listens for and routes"
        " incoming GET, POST, or PUT requests from clients."
    ),
    ("http_rest", "outward"): (
        "Makes an outgoing HTTP or REST request to a remote URL,"
        " such as GET, POST, or PUT to an external web API."
    ),
    ("soap", "inward"): (
        "Implements a SOAP web service endpoint annotated with @WebService"
        " or @WebMethod that receives and processes incoming SOAP XML requests."
    ),
    ("soap", "outward"): (
        "Calls a remote SOAP web service using a WSDL-generated client"
        " proxy with XML envelope serialization."
    ),
    ("messaging", "inward"): (
        "Consumes or listens for incoming messages from a queue or topic"
        " via a message broker subscription."
    ),
    ("messaging", "outward"): (
        "Produces and sends an outgoing message to a queue or topic"
        " via a message broker."
    ),
    ("socket", "inward"): (
        "Binds to a network port and listens for incoming TCP, UDP,"
        " or WebSocket connections from clients."
    ),
    ("socket", "outward"): (
        "Opens an outgoing TCP, UDP, or WebSocket connection by connecting"
        " to a remote host and port."
    ),
    ("database", "inward"): (
        "Defines a database entity, repository, or ORM model class"
        " that maps to and exposes a database table or collection."
    ),
    ("database", "outward"): (
        "Executes a SQL query, ORM find, or database call to read"
        " or write data in a relational database or document store."
    ),
    ("file_io", "inward"): (
        "Accepts an incoming file upload from a client via a multipart"
        " form or upload endpoint."
    ),
    ("file_io", "outward"): (
        "Opens, reads, or writes a file on the local filesystem using"
        " file handles, streams, or path-based I/O."
    ),
    ("grpc", "inward"): (
        "Implements a gRPC service method that handles incoming remote"
        " procedure calls from clients."
    ),
    ("grpc", "outward"): (
        "Creates a gRPC client channel and stub to make an outgoing"
        " remote procedure call to a gRPC service."
    ),
    ("graphql", "inward"): (
        "Defines a GraphQL schema resolver or query field that handles"
        " incoming queries or mutations from clients."
    ),
    ("graphql", "outward"): (
        "Sends a GraphQL query or mutation as a client to a remote"
        " GraphQL API endpoint."
    ),
    ("email", "inward"): (
        "Receives or retrieves incoming email messages from a mail"
        " server using IMAP or POP3 protocol."
    ),
    ("email", "outward"): (
        "Sends an outgoing email message to a recipient via an SMTP server."
    ),
    ("caching", "inward"): (
        "Annotates a method or class as cacheable so its results are"
        " stored and served from cache on subsequent calls."
    ),
    ("caching", "outward"): (
        "Reads from or writes data to an external cache store like"
        " Redis or Memcached using get/set operations."
    ),
    ("sse_streaming", "inward"): (
        "Exposes a server-sent events endpoint that pushes event streams"
        " to connected clients with text/event-stream content type."
    ),
    ("sse_streaming", "outward"): (
        "Connects to a server-sent events endpoint as a client using"
        " EventSource to receive streaming event data."
    ),
    ("scheduling", "inward"): (
        "Annotates a method with @Scheduled or cron decorator so it is"
        " invoked automatically at configured times or intervals."
    ),
    ("scheduling", "outward"): (
        "Programmatically creates and submits a new scheduled job or timer"
        " to a task scheduler or cron system."
    ),
    ("ftp_sftp", "inward"): (
        "Configures an FTP or SFTP server endpoint that accepts incoming"
        " file transfer connections from clients."
    ),
    ("ftp_sftp", "outward"): (
        "Connects to a remote FTP or SFTP server as a client and uploads"
        " or downloads files."
    ),
}

# Code fragments tagged with (language, integration_type, direction)
FRAGMENTS = {
    # ═══════════════════════════════════════════════════════════════
    # HTTP_REST
    # ═══════════════════════════════════════════════════════════════
    # INWARD — route handlers / controllers
    ("Java", "http_rest", "inward"): (
        '@GetMapping("/users")\n'
        "public List<User> getUsers() { return userService.findAll(); }"
    ),
    ("Python", "http_rest", "inward"): (
        '@app.get("/users")\n' "async def get_users():\n" "    return await User.all()"
    ),
    ("TypeScript", "http_rest", "inward"): (
        'app.get("/users", async (req: Request, res: Response) => {\n'
        "    res.json(await getUsers());\n"
        "});"
    ),
    ("Go", "http_rest", "inward"): (
        'http.HandleFunc("/users", func(w http.ResponseWriter, r *http.Request) {\n'
        "    json.NewEncoder(w).Encode(users)\n"
        "})"
    ),
    ("Rust", "http_rest", "inward"): (
        '#[get("/users")]\n'
        "async fn get_users(db: Data<Pool>) -> Json<Vec<User>> {\n"
        "    Json(db.fetch_all().await)\n"
        "}"
    ),
    ("C#", "http_rest", "inward"): (
        '[HttpGet("users")]\n'
        "public async Task<IActionResult> GetUsers() {\n"
        "    return Ok(await _service.GetAll());\n"
        "}"
    ),
    ("Ruby", "http_rest", "inward"): ("get '/users' do\n" "  json User.all\n" "end"),
    ("Kotlin", "http_rest", "inward"): (
        '@GetMapping("/users")\n' "fun getUsers(): List<User> = userService.findAll()"
    ),
    # OUTWARD — HTTP client calls
    ("Java", "http_rest", "outward"): (
        "HttpResponse<String> response = HttpClient.newHttpClient()"
        '.send(HttpRequest.newBuilder(URI.create("https://api.example.com/users")).build(),'
        " BodyHandlers.ofString());"
    ),
    ("Python", "http_rest", "outward"): (
        'response = requests.get("https://api.example.com/users")'
    ),
    ("TypeScript", "http_rest", "outward"): (
        'const response = await axios.get<User[]>("https://api.example.com/users");'
    ),
    ("Go", "http_rest", "outward"): (
        'resp, err := http.Get("https://api.example.com/users")'
    ),
    ("Rust", "http_rest", "outward"): (
        'let response = client.get("https://api.example.com/users").send()?;'
    ),
    ("C#", "http_rest", "outward"): (
        'var response = await httpClient.GetAsync("https://api.example.com/users");'
    ),
    ("Ruby", "http_rest", "outward"): (
        'response = Net::HTTP.get_response(URI("https://api.example.com/users"))'
    ),
    ("Kotlin", "http_rest", "outward"): (
        'val response = client.get("https://api.example.com/users")'
    ),
    # ═══════════════════════════════════════════════════════════════
    # SOAP
    # ═══════════════════════════════════════════════════════════════
    ("Java", "soap", "inward"): (
        "@WebService\n"
        "public class UserServiceImpl {\n"
        "    @WebMethod\n"
        "    public User getUser(String userId) { return repo.find(userId); }\n"
        "}"
    ),
    ("C#", "soap", "inward"): (
        "[ServiceContract]\n"
        "public interface IUserService {\n"
        "    [OperationContract]\n"
        "    User GetUser(string userId);\n"
        "}"
    ),
    ("Python", "soap", "inward"): (
        "class UserService(ServiceBase):\n"
        "    @rpc(Unicode, _returns=User)\n"
        "    def get_user(ctx, user_id):\n"
        "        return find_user(user_id)"
    ),
    ("Java", "soap", "outward"): (
        "SOAPMessage response = soapConnection.call(soapMessage, new URL(endpoint));"
    ),
    ("C#", "soap", "outward"): (
        "var client = new ServiceReference.ServiceClient();\n"
        "var result = await client.GetUserAsync(userId);"
    ),
    ("Python", "soap", "outward"): (
        'client = zeep.Client("https://example.com/service?wsdl")\n'
        "result = client.service.GetUser(user_id)"
    ),
    # ═══════════════════════════════════════════════════════════════
    # MESSAGING
    # ═══════════════════════════════════════════════════════════════
    ("Java", "messaging", "inward"): (
        '@KafkaListener(topics = "orders-topic")\n'
        "public void consume(OrderEvent event) { process(event); }"
    ),
    ("Python", "messaging", "inward"): (
        "for message in consumer:\n" "    process(message.value)"
    ),
    ("TypeScript", "messaging", "inward"): (
        'await consumer.subscribe({ topic: "orders" });\n'
        "await consumer.run({ eachMessage: async ({ message }) => { process(message); } });"
    ),
    ("Go", "messaging", "inward"): (
        "msg := <-partitionConsumer.Messages()\n" "processMessage(msg.Value)"
    ),
    ("Java", "messaging", "outward"): (
        'kafkaTemplate.send("orders-topic", orderEvent);'
    ),
    ("Python", "messaging", "outward"): (
        'producer.send("orders-topic", value=json.dumps(order_event).encode())'
    ),
    ("TypeScript", "messaging", "outward"): (
        "await producer.send({ "
        'topic: "orders", messages: [{ value: JSON.stringify(event) }] });'
    ),
    ("Go", "messaging", "outward"): (
        "producer.SendMessage(&sarama.ProducerMessage{"
        'Topic: "orders", Value: sarama.StringEncoder(payload)})'
    ),
    # ═══════════════════════════════════════════════════════════════
    # SOCKET
    # ═══════════════════════════════════════════════════════════════
    ("Java", "socket", "inward"): (
        "ServerSocket serverSocket = new ServerSocket(8080);\n"
        "Socket client = serverSocket.accept();"
    ),
    ("Python", "socket", "inward"): (
        "server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)\n"
        'server.bind(("0.0.0.0", 8080))\n'
        "server.listen(5)"
    ),
    ("Go", "socket", "inward"): (
        'listener, err := net.Listen("tcp", ":8080")\n' "conn, err := listener.Accept()"
    ),
    ("Rust", "socket", "inward"): (
        'let listener = TcpListener::bind("127.0.0.1:8080")?;\n'
        "let (stream, addr) = listener.accept()?;"
    ),
    ("C#", "socket", "inward"): (
        "var listener = new TcpListener(IPAddress.Any, 8080);\n"
        "listener.Start();\n"
        "var client = await listener.AcceptTcpClientAsync();"
    ),
    ("Java", "socket", "outward"): (
        'Socket socket = new Socket("api.example.com", 8080);\n'
        "OutputStream out = socket.getOutputStream();"
    ),
    ("Python", "socket", "outward"): (
        "sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)\n"
        'sock.connect(("api.example.com", 8080))'
    ),
    ("Go", "socket", "outward"): (
        'conn, err := net.Dial("tcp", "api.example.com:8080")'
    ),
    ("Rust", "socket", "outward"): (
        'let mut stream = TcpStream::connect("api.example.com:8080")?;'
    ),
    ("C#", "socket", "outward"): (
        'var client = new TcpClient("api.example.com", 8080);'
    ),
    # ═══════════════════════════════════════════════════════════════
    # GRPC
    # ═══════════════════════════════════════════════════════════════
    ("Java", "grpc", "inward"): (
        "@Override\n"
        "public void sayHello(HelloRequest request,"
        " StreamObserver<HelloReply> responseObserver) {\n"
        '    responseObserver.onNext(HelloReply.newBuilder().setMessage("Hello").build());\n'
        "    responseObserver.onCompleted();\n"
        "}"
    ),
    ("Python", "grpc", "inward"): (
        "class GreeterServicer(greeter_pb2_grpc.GreeterServicer):\n"
        "    def SayHello(self, request, context):\n"
        '        return greeter_pb2.HelloReply(message="Hello")'
    ),
    ("Go", "grpc", "inward"): (
        "func (s *server) SayHello(ctx context.Context,"
        " req *pb.HelloRequest) (*pb.HelloReply, error) {\n"
        '    return &pb.HelloReply{Message: "Hello"}, nil\n'
        "}"
    ),
    ("Rust", "grpc", "inward"): (
        "#[tonic::async_trait]\n"
        "impl Greeter for MyGreeter {\n"
        "    async fn say_hello(&self, request: Request<HelloRequest>)"
        " -> Result<Response<HelloReply>, Status> {\n"
        '        Ok(Response::new(HelloReply { message: "Hello".into() }))\n'
        "    }\n"
        "}"
    ),
    ("Java", "grpc", "outward"): (
        'ManagedChannel channel = ManagedChannelBuilder.forAddress("localhost", 50051).build();\n'
        "GreeterGrpc.GreeterBlockingStub stub = GreeterGrpc.newBlockingStub(channel);\n"
        "HelloReply reply = stub.sayHello(HelloRequest.newBuilder().setName(name).build());"
    ),
    ("Python", "grpc", "outward"): (
        'channel = grpc.insecure_channel("localhost:50051")\n'
        "stub = greeter_pb2_grpc.GreeterStub(channel)\n"
        'response = stub.SayHello(greeter_pb2.HelloRequest(name="World"))'
    ),
    ("Go", "grpc", "outward"): (
        'conn, err := grpc.Dial("localhost:50051", grpc.WithInsecure())\n'
        "client := pb.NewGreeterClient(conn)\n"
        "reply, err := client.SayHello(ctx, &pb.HelloRequest{Name: name})"
    ),
    ("Rust", "grpc", "outward"): (
        'let mut client = GreeterClient::connect("http://localhost:50051").await?;\n'
        "let response = client.say_hello(HelloRequest { name: name.into() }).await?;"
    ),
    # ═══════════════════════════════════════════════════════════════
    # GRAPHQL
    # ═══════════════════════════════════════════════════════════════
    ("Java", "graphql", "inward"): (
        "@QueryMapping\n" "public List<User> users() { return userService.findAll(); }"
    ),
    ("Python", "graphql", "inward"): (
        "class Query(graphene.ObjectType):\n"
        "    users = graphene.List(UserType)\n"
        "    def resolve_users(self, info):\n"
        "        return User.objects.all()"
    ),
    ("TypeScript", "graphql", "inward"): (
        "const resolvers = {\n"
        "    Query: {\n"
        "        users: async () => await prisma.user.findMany()\n"
        "    }\n"
        "};"
    ),
    ("Ruby", "graphql", "inward"): (
        "field :users, [Types::UserType], null: false\n"
        "def users\n"
        "  User.all\n"
        "end"
    ),
    ("Java", "graphql", "outward"): (
        'graphQlClient.document("{ users { id name } }")'
        '.retrieve("users").toEntityList(User.class);'
    ),
    ("Python", "graphql", "outward"): (
        "result = client.execute(gql('{ users { id name } }'))"
    ),
    ("TypeScript", "graphql", "outward"): (
        "const { data } = await client.query({ query: GET_USERS });"
    ),
    ("Kotlin", "graphql", "outward"): (
        "val response = apolloClient.query(GetUsersQuery()).execute()"
    ),
    # ═══════════════════════════════════════════════════════════════
    # EMAIL
    # ═══════════════════════════════════════════════════════════════
    ("Java", "email", "inward"): (
        'Store store = session.getStore("imaps");\n'
        "store.connect(host, user, password);\n"
        'Folder inbox = store.getFolder("INBOX");\n'
        "Message[] messages = inbox.getMessages();"
    ),
    ("Python", "email", "inward"): (
        'mail = imaplib.IMAP4_SSL("imap.example.com")\n'
        "mail.login(user, password)\n"
        'mail.select("inbox")\n'
        'status, messages = mail.search(None, "ALL")'
    ),
    ("Ruby", "email", "inward"): (
        "Mail.defaults do\n"
        '  retriever_method :imap, address: "imap.example.com"\n'
        "end\n"
        "emails = Mail.all"
    ),
    ("Java", "email", "outward"): (
        "MimeMessage msg = new MimeMessage(session);\n"
        'msg.setSubject("Hello");\n'
        "Transport.send(msg);"
    ),
    ("Python", "email", "outward"): (
        'smtp = smtplib.SMTP("smtp.example.com", 587)\n'
        "smtp.starttls()\n"
        "smtp.send_message(msg)"
    ),
    ("TypeScript", "email", "outward"): (
        "await transporter.sendMail({\n"
        '    from: sender, to: recipient, subject: "Hello", html: body\n'
        "});"
    ),
    ("Ruby", "email", "outward"): (
        "ActionMailer::Base.mail(\n"
        '    to: recipient, subject: "Hello", body: content\n'
        ").deliver_now"
    ),
    # ═══════════════════════════════════════════════════════════════
    # SSE_STREAMING
    # ═══════════════════════════════════════════════════════════════
    ("Java", "sse_streaming", "inward"): (
        "SseEmitter emitter = new SseEmitter();\n"
        'emitter.send(SseEmitter.event().name("update").data(payload));'
    ),
    ("Python", "sse_streaming", "inward"): (
        "async def event_stream():\n"
        "    while True:\n"
        "        data = await get_update()\n"
        '        yield f"data: {json.dumps(data)}\\n\\n"'
    ),
    ("JavaScript", "sse_streaming", "inward"): (
        'res.writeHead(200, { "Content-Type": "text/event-stream" });\n'
        "res.write(`data: ${JSON.stringify(payload)}\\n\\n`);"
    ),
    ("TypeScript", "sse_streaming", "outward"): (
        'const eventSource = new EventSource("/api/events");\n'
        "eventSource.onmessage = (event) => { process(JSON.parse(event.data)); };"
    ),
    ("JavaScript", "sse_streaming", "outward"): (
        'const es = new EventSource("https://api.example.com/stream");\n'
        'es.addEventListener("update", (e) => { handleUpdate(JSON.parse(e.data)); });'
    ),
    # ═══════════════════════════════════════════════════════════════
    # FTP_SFTP
    # ═══════════════════════════════════════════════════════════════
    ("Java", "ftp_sftp", "outward"): (
        'ChannelSftp sftp = (ChannelSftp) session.openChannel("sftp");\n'
        "sftp.connect();\n"
        "sftp.put(localFile, remotePath);"
    ),
    ("Python", "ftp_sftp", "outward"): (
        "sftp = paramiko.SFTPClient.from_transport(transport)\n"
        "sftp.put(local_path, remote_path)"
    ),
    ("Go", "ftp_sftp", "outward"): (
        "client, err := sftp.NewClient(sshConn)\n"
        "dstFile, err := client.Create(remotePath)\n"
        "dstFile.Write(data)"
    ),
    ("Ruby", "ftp_sftp", "outward"): (
        "Net::SFTP.start(host, user, password: pass) do |sftp|\n"
        "  sftp.upload!(local_path, remote_path)\n"
        "end"
    ),
    # ═══════════════════════════════════════════════════════════════
    # DATABASE
    # ═══════════════════════════════════════════════════════════════
    # INWARD — entity/repository definitions
    ("Java", "database", "inward"): (
        "@Entity\n"
        '@Table(name = "users")\n'
        "public class User {\n"
        "    @Id @GeneratedValue\n"
        "    private Long id;\n"
        "    private String name;\n"
        "}"
    ),
    ("Python", "database", "inward"): (
        "class User(db.Model):\n"
        "    __tablename__ = 'users'\n"
        "    id = db.Column(db.Integer, primary_key=True)\n"
        "    name = db.Column(db.String(100))"
    ),
    ("TypeScript", "database", "inward"): (
        "@Entity()\n"
        "export class User {\n"
        "    @PrimaryGeneratedColumn()\n"
        "    id: number;\n"
        "    @Column()\n"
        "    name: string;\n"
        "}"
    ),
    ("Ruby", "database", "inward"): (
        "class User < ApplicationRecord\n"
        "  has_many :orders\n"
        "  validates :name, presence: true\n"
        "end"
    ),
    # OUTWARD — queries and operations
    ("Java", "database", "outward"): (
        'ResultSet rs = statement.executeQuery("SELECT * FROM users WHERE active = true");'
    ),
    ("Python", "database", "outward"): (
        'cursor.execute("SELECT * FROM users WHERE active = %s", (True,))'
    ),
    ("TypeScript", "database", "outward"): (
        "const users = await prisma.user.findMany({ where: { active: true } });"
    ),
    ("Go", "database", "outward"): (
        'rows, err := db.Query("SELECT * FROM users WHERE active = $1", true)'
    ),
    # ═══════════════════════════════════════════════════════════════
    # CACHING
    # ═══════════════════════════════════════════════════════════════
    ("Java", "caching", "inward"): (
        '@Cacheable("users")\n'
        "public User findUser(String id) {\n"
        "    return userRepository.findById(id);\n"
        "}"
    ),
    ("Python", "caching", "inward"): (
        "@cache.cached(timeout=300, key_prefix='user')\n"
        "def get_user(user_id):\n"
        "    return User.query.get(user_id)"
    ),
    ("Java", "caching", "outward"): (
        'redisTemplate.opsForValue().set("user:123", userJson);'
    ),
    ("Python", "caching", "outward"): (
        'redis_client.set("user:123", json.dumps(user_data))'
    ),
    ("TypeScript", "caching", "outward"): (
        'await redis.set("user:123", JSON.stringify(userData));'
    ),
    # ═══════════════════════════════════════════════════════════════
    # SCHEDULING
    # ═══════════════════════════════════════════════════════════════
    ("Java", "scheduling", "inward"): (
        '@Scheduled(cron = "0 0 * * * *")\n'
        "public void runHourlyJob() {\n"
        "    processData();\n"
        "}"
    ),
    ("Kotlin", "scheduling", "inward"): (
        "@Scheduled(fixedRate = 60000)\n" "fun processQueue() { handlePendingItems() }"
    ),
    ("Python", "scheduling", "outward"): (
        "scheduler.add_job(process_data, 'cron', hour='*/2')"
    ),
    ("Go", "scheduling", "outward"): (
        "c := cron.New()\n" 'c.AddFunc("@every 1h", func() { process() })\n' "c.Start()"
    ),
    ("TypeScript", "scheduling", "outward"): (
        'cron.schedule("*/5 * * * *", () => processQueue());'
    ),
    # ═══════════════════════════════════════════════════════════════
    # FILE_IO
    # ═══════════════════════════════════════════════════════════════
    ("Java", "file_io", "inward"): (
        '@PostMapping("/upload")\n'
        "public ResponseEntity<String> handleUpload("
        '@RequestParam("file") MultipartFile file) {\n'
        "    storageService.store(file);\n"
        '    return ResponseEntity.ok("uploaded");\n'
        "}"
    ),
    ("Python", "file_io", "inward"): (
        '@app.post("/upload")\n'
        "async def upload_file(file: UploadFile):\n"
        "    contents = await file.read()\n"
        "    save_to_disk(contents)"
    ),
    ("TypeScript", "file_io", "inward"): (
        "app.post('/upload', upload.single('file'), (req, res) => {\n"
        "    const file = req.file;\n"
        "    processUpload(file.path);\n"
        "});"
    ),
    ("Java", "file_io", "outward"): (
        'BufferedWriter writer = new BufferedWriter(new FileWriter("output.csv"));\n'
        "writer.write(contents);"
    ),
    ("Python", "file_io", "outward"): (
        'with open("data/output.csv", "w") as f:\n' "    f.write(contents)"
    ),
    ("Go", "file_io", "outward"): ('err := os.WriteFile("output.csv", data, 0644)'),
    ("Rust", "file_io", "outward"): (
        'let mut file = File::create("output.csv")?;\n'
        "file.write_all(contents.as_bytes())?;"
    ),
}


def cosine(a, b):
    dot = sum(x * y for x, y in zip(a, b))
    ma = math.sqrt(sum(x * x for x in a))
    mb = math.sqrt(sum(y * y for y in b))
    return dot / (ma * mb) if ma and mb else 0.0


def embed_batch(inputs):
    headers = {
        "Content-Type": "application/json",
        "Authorization": f"Bearer {TOKEN}",
    }
    resp = requests.post(ENDPOINT, json={"inputs": inputs}, headers=headers)
    resp.raise_for_status()
    return resp.json()


def main():
    # Embed directional descriptions
    desc_keys = list(DIRECTIONAL_DESCRIPTIONS.keys())
    desc_texts = [DIRECTIONAL_DESCRIPTIONS[k] for k in desc_keys]
    print("Embedding directional descriptions...", file=sys.stderr)
    desc_embeddings = embed_batch(desc_texts)

    # Embed code fragments in batches
    frag_keys = list(FRAGMENTS.keys())
    frag_texts = [FRAGMENTS[k] for k in frag_keys]

    BATCH_SIZE = 32
    frag_embeddings = []
    for i in range(0, len(frag_texts), BATCH_SIZE):
        batch = frag_texts[i : i + BATCH_SIZE]
        print(
            f"Embedding fragments {i+1}-{i+len(batch)} of {len(frag_texts)}...",
            file=sys.stderr,
        )
        frag_embeddings.extend(embed_batch(batch))

    # Classify each fragment
    full_correct = 0
    direction_correct = 0
    direction_total = 0
    total = 0
    results_by_type = {}

    for fi, (lang, expected_type, expected_dir) in enumerate(frag_keys):
        vec_code = frag_embeddings[fi]
        scores = {
            desc_keys[di]: cosine(vec_code, desc_embeddings[di])
            for di in range(len(desc_keys))
        }
        predicted_key = max(scores, key=scores.get)
        predicted_type, predicted_dir = predicted_key

        is_full_correct = (
            predicted_type == expected_type and predicted_dir == expected_dir
        )
        full_correct += int(is_full_correct)
        total += 1

        # Direction accuracy: compare against same-type descriptions only
        same_type_scores = {k: v for k, v in scores.items() if k[0] == expected_type}
        dir_predicted = max(same_type_scores, key=same_type_scores.get)[1]
        is_dir_correct = dir_predicted == expected_dir
        direction_correct += int(is_dir_correct)
        direction_total += 1

        key = (expected_type, expected_dir)
        if key not in results_by_type:
            results_by_type[key] = {
                "correct_full": 0,
                "correct_dir": 0,
                "total": 0,
                "misses": [],
            }
        results_by_type[key]["total"] += 1
        results_by_type[key]["correct_full"] += int(is_full_correct)
        results_by_type[key]["correct_dir"] += int(is_dir_correct)
        if not is_full_correct:
            inward_score = scores.get((expected_type, "inward"), 0)
            outward_score = scores.get((expected_type, "outward"), 0)
            results_by_type[key]["misses"].append(
                (
                    lang,
                    predicted_type,
                    predicted_dir,
                    scores[(expected_type, expected_dir)],
                    scores[predicted_key],
                    inward_score,
                    outward_score,
                )
            )

    # Print per-fragment results
    print()
    print(
        f"{'Language':<12} {'Type':<16} {'Expected':<8} {'Pred Type':<16}"
        f" {'Pred Dir':<8} {'Full':>4}  {'Dir':>3}"
        f"  {'In Score':>8} {'Out Score':>9}"
    )
    print("-" * 100)
    for fi, (lang, expected_type, expected_dir) in enumerate(frag_keys):
        vec_code = frag_embeddings[fi]
        scores = {
            desc_keys[di]: cosine(vec_code, desc_embeddings[di])
            for di in range(len(desc_keys))
        }
        predicted_key = max(scores, key=scores.get)
        predicted_type, predicted_dir = predicted_key

        same_type_scores = {k: v for k, v in scores.items() if k[0] == expected_type}
        dir_predicted = max(same_type_scores, key=same_type_scores.get)[1]

        full_ok = (
            "Y"
            if (predicted_type == expected_type and predicted_dir == expected_dir)
            else "N"
        )
        dir_ok = "Y" if dir_predicted == expected_dir else "N"

        in_score = scores.get((expected_type, "inward"), 0)
        out_score = scores.get((expected_type, "outward"), 0)

        print(
            f"{lang:<12} {expected_type:<16} {expected_dir:<8}"
            f" {predicted_type:<16} {predicted_dir:<8} {full_ok:>4}  {dir_ok:>3}"
            f"  {in_score:>8.4f} {out_score:>9.4f}"
        )

    # Summary
    print("-" * 100)
    full_pct = full_correct / total * 100 if total else 0
    dir_pct = direction_correct / direction_total * 100 if direction_total else 0
    print(
        f"\nFull accuracy (type + direction):  {full_correct}/{total} ({full_pct:.1f}%)"
    )
    print(
        f"Direction-only accuracy:           {direction_correct}/{direction_total} ({dir_pct:.1f}%)"
    )

    # Per-type breakdown
    print(f"\n{'Type':<16} {'Direction':<8} {'Full':>6} {'Dir':>6} {'Total':>5}")
    print("-" * 48)
    for itype, idir in sorted(results_by_type.keys()):
        r = results_by_type[(itype, idir)]
        full_pct = r["correct_full"] / r["total"] * 100
        dir_pct = r["correct_dir"] / r["total"] * 100
        print(
            f"{itype:<16} {idir:<8} {full_pct:>5.0f}% {dir_pct:>5.0f}% {r['total']:>5}"
        )

    # Print misclassifications
    all_misses = [
        ((itype, idir), *miss)
        for (itype, idir), r in results_by_type.items()
        for miss in r["misses"]
    ]
    if all_misses:
        print(f"\nMisclassifications ({len(all_misses)}):")
        print(
            f"{'Type':<16} {'Dir':<7} {'Lang':<12} {'Pred Type':<16}"
            f" {'Pred Dir':<8} {'Exp Score':>9} {'Pred Score':>10}"
            f" {'In Score':>9} {'Out Score':>9}"
        )
        print("-" * 105)
        for (
            (itype, idir),
            lang,
            pred_type,
            pred_dir,
            exp_sc,
            pred_sc,
            in_sc,
            out_sc,
        ) in all_misses:
            print(
                f"{itype:<16} {idir:<7} {lang:<12} {pred_type:<16}"
                f" {pred_dir:<8} {exp_sc:>9.4f} {pred_sc:>10.4f}"
                f" {in_sc:>9.4f} {out_sc:>9.4f}"
            )


if __name__ == "__main__":
    main()
