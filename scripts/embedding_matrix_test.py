"""Test nomic-embed-code classification accuracy across all (language, integration_type) pairs."""

import json
import math
import os
import sys
import requests

ENDPOINT = os.environ["HUGGING_FACE_URL"]
TOKEN = os.environ["HUGGING_FACE_API_TOKEN"]

DESCRIPTIONS = {
    "http_rest": "Makes an HTTP or REST request to a URL, such as GET, POST, or PUT to a web API endpoint.",
    "soap": "Calls a SOAP web service using a WSDL-generated client proxy with XML envelope serialization.",
    "messaging": "Sends a message to a queue or topic via a message broker.",
    "socket": "Opens a TCP, UDP, or WebSocket connection by binding or connecting to a network port to accept or send data.",
    "database": "Executes a SQL query or ORM operation against a relational database table or document store.",
    "file_io": "Opens, reads, or writes a file on the local filesystem using file handles, streams, or path-based I/O.",
    "grpc": "Makes a gRPC remote procedure call over an HTTP/2 channel to a protobuf service.",
    "graphql": "Sends a GraphQL query or mutation using a schema with types, fields, and resolvers.",
    "email": "Sends an email message via an SMTP server.",
    "caching": "Reads or writes data to a cache store like Redis.",
    "sse_streaming": "Pushes server-sent events to a client using text/event-stream content type with data: prefix lines.",
    "scheduling": "Schedules a task to run at a specified time or interval.",
    "ftp_sftp": "Transfers a file to a remote server via FTP or SFTP.",
}

# Representative code fragments for each (language, integration_type)
FRAGMENTS = {
    # ── Java ──
    (
        "Java",
        "http_rest",
    ): "HttpResponse<String> response = HttpClient.newHttpClient().send(request, BodyHandlers.ofString());",
    (
        "Java",
        "soap",
    ): "SOAPMessage response = soapConnection.call(soapMessage, new URL(endpoint));",
    ("Java", "messaging"): 'kafkaTemplate.send("orders-topic", orderEvent);',
    (
        "Java",
        "socket",
    ): "ServerSocket serverSocket = new ServerSocket(8080);\nSocket client = serverSocket.accept();",
    (
        "Java",
        "database",
    ): 'ResultSet rs = statement.executeQuery("SELECT * FROM users WHERE active = true");',
    (
        "Java",
        "file_io",
    ): 'BufferedWriter writer = new BufferedWriter(new FileWriter("output.csv"));',
    (
        "Java",
        "grpc",
    ): 'ManagedChannel channel = ManagedChannelBuilder.forAddress("localhost", 9090).build();',
    (
        "Java",
        "graphql",
    ): 'graphQlClient.document(query).retrieve("users").toEntityList(User.class);',
    ("Java", "email"): "Transport.send(mimeMessage);",
    ("Java", "caching"): 'redisTemplate.opsForValue().set("user:123", userJson);',
    (
        "Java",
        "sse_streaming",
    ): "SseEmitter emitter = new SseEmitter();\nemitter.send(SseEmitter.event().data(payload));",
    (
        "Java",
        "scheduling",
    ): '@Scheduled(cron = "0 0 * * * *")\npublic void runHourlyJob() { process(); }',
    (
        "Java",
        "ftp_sftp",
    ): 'ChannelSftp sftp = (ChannelSftp) session.openChannel("sftp");\nsftp.put(localFile, remotePath);',
    # ── Python ──
    ("Python", "http_rest"): 'response = requests.get("https://api.example.com/users")',
    (
        "Python",
        "soap",
    ): 'client = zeep.Client("https://example.com/service?wsdl")\nresult = client.service.GetUser(user_id)',
    (
        "Python",
        "messaging",
    ): 'producer.send("orders-topic", value=json.dumps(order_event).encode())',
    (
        "Python",
        "socket",
    ): 'sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)\nsock.connect(("localhost", 8080))',
    (
        "Python",
        "database",
    ): 'cursor.execute("SELECT * FROM users WHERE active = %s", (True,))',
    (
        "Python",
        "file_io",
    ): 'with open("data/output.csv", "w") as f:\n    f.write(contents)',
    (
        "Python",
        "grpc",
    ): 'channel = grpc.insecure_channel("localhost:50051")\nstub = greeter_pb2_grpc.GreeterStub(channel)',
    ("Python", "graphql"): "result = client.execute(gql('{ users { id name } }'))",
    (
        "Python",
        "email",
    ): 'smtp = smtplib.SMTP("smtp.example.com", 587)\nsmtp.send_message(msg)',
    ("Python", "caching"): 'redis_client.set("user:123", json.dumps(user_data))',
    ("Python", "sse_streaming"): 'yield f"data: {json.dumps(event)}\\n\\n"',
    ("Python", "scheduling"): 'scheduler.add_job(process_data, "cron", hour="*/2")',
    (
        "Python",
        "ftp_sftp",
    ): "sftp = paramiko.SFTPClient.from_transport(transport)\nsftp.put(local_path, remote_path)",
    # ── TypeScript ──
    (
        "TypeScript",
        "http_rest",
    ): 'const response = await axios.get<User[]>("https://api.example.com/users");',
    (
        "TypeScript",
        "soap",
    ): "const client = await soap.createClientAsync(wsdlUrl);\nconst [result] = await client.GetUserAsync({userId});",
    (
        "TypeScript",
        "messaging",
    ): 'await producer.send({ topic: "orders", messages: [{ value: JSON.stringify(event) }] });',
    (
        "TypeScript",
        "socket",
    ): 'const socket = new net.Socket();\nsocket.connect(8080, "127.0.0.1");\nsocket.write(Buffer.from(payload));',
    (
        "TypeScript",
        "database",
    ): "const users = await prisma.user.findMany({ where: { active: true } });",
    (
        "TypeScript",
        "file_io",
    ): 'await fs.promises.writeFile("output.csv", contents, "utf-8");',
    (
        "TypeScript",
        "grpc",
    ): 'const client = new GreeterClient("localhost:50051", grpc.credentials.createInsecure());',
    (
        "TypeScript",
        "graphql",
    ): "const { data } = await client.query({ query: GET_USERS });",
    (
        "TypeScript",
        "email",
    ): "await transporter.sendMail({ from: sender, to: recipient, subject, html: body });",
    ("TypeScript", "caching"): 'await redis.set("user:123", JSON.stringify(userData));',
    (
        "TypeScript",
        "sse_streaming",
    ): 'const eventSource = new EventSource("/api/events");',
    ("TypeScript", "scheduling"): 'cron.schedule("*/5 * * * *", () => processQueue());',
    ("TypeScript", "ftp_sftp"): "await sftp.put(localPath, remotePath);",
    # ── JavaScript ──
    (
        "JavaScript",
        "http_rest",
    ): 'const response = await fetch("https://api.example.com/users");',
    (
        "JavaScript",
        "soap",
    ): "soap.createClient(wsdlUrl, function(err, client) { client.GetUser({userId}, callback); });",
    (
        "JavaScript",
        "messaging",
    ): 'channel.sendToQueue("tasks", Buffer.from(JSON.stringify(message)));',
    (
        "JavaScript",
        "socket",
    ): 'const io = require("socket.io")(server);\nio.on("connection", (socket) => { });',
    (
        "JavaScript",
        "database",
    ): 'const users = await db.query("SELECT * FROM users WHERE active = $1", [true]);',
    ("JavaScript", "file_io"): 'fs.writeFileSync("output.csv", contents, "utf-8");',
    (
        "JavaScript",
        "grpc",
    ): 'const client = new hello_proto.Greeter("localhost:50051", grpc.credentials.createInsecure());',
    (
        "JavaScript",
        "graphql",
    ): 'const result = await graphql({ schema, source: "{ users { id name } }" });',
    (
        "JavaScript",
        "email",
    ): 'transporter.sendMail({ from: sender, to: recipient, subject: "Hello", html: body });',
    (
        "JavaScript",
        "caching",
    ): 'await client.set("user:123", JSON.stringify(userData));',
    (
        "JavaScript",
        "sse_streaming",
    ): 'res.writeHead(200, { "Content-Type": "text/event-stream" });\nres.write(`data: ${payload}\\n\\n`);',
    (
        "JavaScript",
        "scheduling",
    ): 'cron.schedule("*/5 * * * *", () => { processQueue(); });',
    (
        "JavaScript",
        "ftp_sftp",
    ): "await sftp.connect(config);\nawait sftp.put(localPath, remotePath);",
    # ── Go ──
    ("Go", "http_rest"): 'resp, err := http.Get("https://api.example.com/users")',
    ("Go", "soap"): 'client := gowsdl.NewClient("https://example.com/service?wsdl")',
    (
        "Go",
        "messaging",
    ): 'producer.SendMessage(&sarama.ProducerMessage{Topic: "orders", Value: sarama.StringEncoder(payload)})',
    ("Go", "socket"): 'conn, err := net.Dial("tcp", "localhost:8080")',
    (
        "Go",
        "database",
    ): 'rows, err := db.Query("SELECT * FROM users WHERE active = $1", true)',
    ("Go", "file_io"): 'err := os.WriteFile("output.csv", data, 0644)',
    ("Go", "grpc"): 'conn, err := grpc.Dial("localhost:50051", grpc.WithInsecure())',
    (
        "Go",
        "graphql",
    ): "req := graphql.NewRequest(`{ users { id name } }`)\nclient.Run(ctx, req, &resp)",
    (
        "Go",
        "email",
    ): 'err := smtp.SendMail("smtp.example.com:587", auth, from, to, msg)',
    ("Go", "caching"): 'err := rdb.Set(ctx, "user:123", userJSON, 0).Err()',
    (
        "Go",
        "sse_streaming",
    ): 'flusher.Flush()\nfmt.Fprintf(w, "data: %s\\n\\n", payload)',
    (
        "Go",
        "scheduling",
    ): 'c := cron.New()\nc.AddFunc("@every 1h", func() { process() })',
    (
        "Go",
        "ftp_sftp",
    ): "client, err := sftp.NewClient(sshConn)\nclient.Create(remotePath)",
    # ── Rust ──
    (
        "Rust",
        "http_rest",
    ): 'let response = client.get("https://api.example.com/users").send()?;',
    ("Rust", "soap"): "let response = soap_client.send_request(envelope)?;",
    (
        "Rust",
        "messaging",
    ): 'producer.send(FutureRecord::to("orders-topic").payload(payload.as_bytes())).await?;',
    (
        "Rust",
        "socket",
    ): 'let listener = TcpListener::bind("127.0.0.1:8080")?;\nlet (stream, addr) = listener.accept()?;',
    (
        "Rust",
        "database",
    ): 'let rows = sqlx::query("SELECT * FROM users WHERE active = true").fetch_all(&pool).await?;',
    (
        "Rust",
        "file_io",
    ): 'let mut file = File::create("output.csv")?;\nfile.write_all(contents.as_bytes())?;',
    (
        "Rust",
        "grpc",
    ): 'let mut client = GreeterClient::connect("http://localhost:50051").await?;',
    (
        "Rust",
        "graphql",
    ): "let response = surf::post(url).body(Body::from(query)).await?;",
    (
        "Rust",
        "email",
    ): 'let mailer = SmtpTransport::relay("smtp.example.com")?.build();\nmailer.send(&email)?;',
    ("Rust", "caching"): 'conn.set::<_, _, ()>("user:123", user_json).await?;',
    ("Rust", "sse_streaming"): "Sse::new(stream.map(|msg| Event::default().data(msg)))",
    (
        "Rust",
        "scheduling",
    ): 'let mut scheduler = JobScheduler::new();\nscheduler.add(Job::new("0 */2 * * *", || { process(); })?)?;',
    (
        "Rust",
        "ftp_sftp",
    ): "let mut sftp = sess.sftp()?;\nlet mut remote_file = sftp.create(Path::new(remote_path))?;",
    # ── C# ──
    (
        "C#",
        "http_rest",
    ): 'var response = await httpClient.GetAsync("https://api.example.com/users");',
    (
        "C#",
        "soap",
    ): "var client = new ServiceReference.ServiceClient();\nvar result = await client.GetUserAsync(userId);",
    (
        "C#",
        "messaging",
    ): 'await producer.ProduceAsync("orders-topic", new Message<string, string> { Value = payload });',
    (
        "C#",
        "socket",
    ): "var listener = new TcpListener(IPAddress.Any, 8080);\nlistener.Start();",
    (
        "C#",
        "database",
    ): "var users = await context.Users.Where(u => u.Active).ToListAsync();",
    ("C#", "file_io"): 'await File.WriteAllTextAsync("output.csv", contents);',
    (
        "C#",
        "grpc",
    ): 'var channel = GrpcChannel.ForAddress("https://localhost:5001");\nvar client = new Greeter.GreeterClient(channel);',
    (
        "C#",
        "graphql",
    ): 'var result = await client.SendQueryAsync<UsersResponse>(new GraphQLRequest { Query = "{ users { id } }" });',
    (
        "C#",
        "email",
    ): "await smtpClient.SendMailAsync(new MailMessage(from, to, subject, body));",
    (
        "C#",
        "caching",
    ): 'await distributedCache.SetStringAsync("user:123", JsonSerializer.Serialize(user));',
    (
        "C#",
        "sse_streaming",
    ): 'await response.WriteAsync($"data: {payload}\n\n");\nawait response.Body.FlushAsync();',
    (
        "C#",
        "scheduling",
    ): 'RecurringJob.AddOrUpdate("hourly-job", () => ProcessData(), Cron.Hourly);',
    (
        "C#",
        "ftp_sftp",
    ): "using var sftp = new SftpClient(host, username, password);\nsftp.UploadFile(fileStream, remotePath);",
    # ── Kotlin ──
    (
        "Kotlin",
        "http_rest",
    ): 'val response = client.get("https://api.example.com/users")',
    ("Kotlin", "soap"): "val result = soapClient.call(soapAction, envelope)",
    ("Kotlin", "messaging"): 'kafkaTemplate.send("orders-topic", orderEvent)',
    (
        "Kotlin",
        "socket",
    ): "val serverSocket = ServerSocket(8080)\nval client = serverSocket.accept()",
    (
        "Kotlin",
        "database",
    ): "val users = transaction { Users.select { Users.active eq true }.toList() }",
    ("Kotlin", "file_io"): 'File("output.csv").writeText(contents)',
    (
        "Kotlin",
        "grpc",
    ): 'val channel = ManagedChannelBuilder.forAddress("localhost", 50051).usePlaintext().build()',
    (
        "Kotlin",
        "graphql",
    ): "val response = apolloClient.query(GetUsersQuery()).execute()",
    ("Kotlin", "email"): "javaMailSender.send(mimeMessage)",
    ("Kotlin", "caching"): 'redisTemplate.opsForValue().set("user:123", userJson)',
    (
        "Kotlin",
        "sse_streaming",
    ): "val emitter = SseEmitter()\nemitter.send(SseEmitter.event().data(payload))",
    ("Kotlin", "scheduling"): "@Scheduled(fixedRate = 60000)\nfun processQueue() { }",
    ("Kotlin", "ftp_sftp"): "channelSftp.put(localPath, remotePath)",
    # ── Scala ──
    (
        "Scala",
        "http_rest",
    ): 'val response = sttp.basicRequest.get(uri"https://api.example.com/users").send(backend)',
    (
        "Scala",
        "soap",
    ): "val binding = new scalaxb.Soap11Clients with scalaxb.DispatchHttpClients {}",
    (
        "Scala",
        "messaging",
    ): 'producer.send(new ProducerRecord[String, String]("orders-topic", payload))',
    (
        "Scala",
        "socket",
    ): 'val serverBinding = Http().newServerAt("0.0.0.0", 8080).bind(route)',
    (
        "Scala",
        "database",
    ): 'val users = sql"SELECT * FROM users WHERE active = true".as[User].result',
    (
        "Scala",
        "file_io",
    ): 'val source = Source.fromFile("input.csv")\nval lines = source.getLines().toList',
    (
        "Scala",
        "grpc",
    ): 'val channel = ManagedChannelBuilder.forAddress("localhost", 50051).usePlaintext().build()',
    (
        "Scala",
        "graphql",
    ): 'val schema = Schema(ObjectType("Query", fields[Unit](Field("users", ListType(UserType), resolve = _.value))))',
    (
        "Scala",
        "email",
    ): 'val mailer = Mailer("smtp.example.com", 587)()\nmailer(envelope)',
    ("Scala", "caching"): 'redisClient.set("user:123", userJson)',
    (
        "Scala",
        "sse_streaming",
    ): "complete(HttpEntity(ContentTypes.`text/event-stream`, source))",
    (
        "Scala",
        "scheduling",
    ): "system.scheduler.scheduleAtFixedRate(0.seconds, 1.hour)(new Runnable { def run() = process() })",
    (
        "Scala",
        "ftp_sftp",
    ): "val sftp = new SFTPClient(sshClient)\nsftp.put(localPath, remotePath)",
    # ── Ruby ──
    (
        "Ruby",
        "http_rest",
    ): 'response = Net::HTTP.get_response(URI("https://api.example.com/users"))',
    (
        "Ruby",
        "soap",
    ): 'client = Savon.client(wsdl: "https://example.com/service?wsdl")\nresponse = client.call(:get_user)',
    ("Ruby", "messaging"): 'exchange.publish(payload.to_json, routing_key: "orders")',
    (
        "Ruby",
        "socket",
    ): 'server = TCPServer.new("0.0.0.0", 8080)\nclient = server.accept',
    ("Ruby", "database"): "users = User.where(active: true).to_a",
    ("Ruby", "file_io"): 'File.write("output.csv", contents)',
    (
        "Ruby",
        "grpc",
    ): 'stub = Helloworld::Greeter::Stub.new("localhost:50051", :this_channel_is_insecure)',
    ("Ruby", "graphql"): "result = Schema.execute(query, variables: { id: 1 })",
    (
        "Ruby",
        "email",
    ): 'ActionMailer::Base.mail(to: recipient, subject: "Hello", body: content).deliver_now',
    ("Ruby", "caching"): 'Rails.cache.write("user:123", user.to_json)',
    ("Ruby", "sse_streaming"): 'response.stream.write("data: #{payload}\n\n")',
    (
        "Ruby",
        "scheduling",
    ): 'every 1.hour do\n  runner "ProcessDataJob.perform_now"\nend',
    (
        "Ruby",
        "ftp_sftp",
    ): "Net::SFTP.start(host, user, password: pass) do |sftp|\n  sftp.upload!(local_path, remote_path)\nend",
    # ── PHP ──
    (
        "PHP",
        "http_rest",
    ): '$response = $client->request("GET", "https://api.example.com/users");',
    (
        "PHP",
        "soap",
    ): '$client = new SoapClient("https://example.com/service?wsdl");\n$result = $client->GetUser($userId);',
    (
        "PHP",
        "messaging",
    ): '$producer->send(new Message(json_encode($orderEvent)), "orders-topic");',
    (
        "PHP",
        "socket",
    ): '$socket = socket_create(AF_INET, SOCK_STREAM, SOL_TCP);\nsocket_connect($socket, "127.0.0.1", 8080);',
    (
        "PHP",
        "database",
    ): '$stmt = $pdo->prepare("SELECT * FROM users WHERE active = ?");\n$stmt->execute([true]);',
    (
        "PHP",
        "file_io",
    ): '$handle = fopen("output.csv", "w");\nfwrite($handle, $contents);\nfclose($handle);',
    (
        "PHP",
        "grpc",
    ): '$client = new GreeterClient("localhost:50051", ["credentials" => ChannelCredentials::createInsecure()]);',
    (
        "PHP",
        "graphql",
    ): "$result = GraphQL::executeQuery($schema, $query, null, null, $variables);",
    (
        "PHP",
        "email",
    ): '$mailer->send((new Email())->from($sender)->to($recipient)->subject("Hello")->html($body));',
    ("PHP", "caching"): '$redis->set("user:123", json_encode($userData));',
    (
        "PHP",
        "sse_streaming",
    ): 'header("Content-Type: text/event-stream");\necho "data: " . json_encode($payload) . "\\n\\n";',
    ("PHP", "scheduling"): '$schedule->command("process:data")->hourly();',
    (
        "PHP",
        "ftp_sftp",
    ): "$sftp->put($remotePath, $localPath, SFTP::SOURCE_LOCAL_FILE);",
    # ── C ──
    (
        "C",
        "http_rest",
    ): 'curl_easy_setopt(curl, CURLOPT_URL, "https://api.example.com/users");\ncurl_easy_perform(curl);',
    ("C", "soap"): "xmlDocPtr doc = xmlParseMemory(soap_response, len);",
    (
        "C",
        "messaging",
    ): "rd_kafka_produce(topic, RD_KAFKA_PARTITION_UA, RD_KAFKA_MSG_F_COPY, payload, len, NULL, 0, NULL);",
    (
        "C",
        "socket",
    ): "int sockfd = socket(AF_INET, SOCK_STREAM, 0);\nconnect(sockfd, (struct sockaddr*)&serv_addr, sizeof(serv_addr));",
    (
        "C",
        "database",
    ): 'rc = sqlite3_exec(db, "SELECT * FROM users WHERE active = 1", callback, 0, &err_msg);',
    (
        "C",
        "file_io",
    ): 'FILE *fp = fopen("output.csv", "w");\nfprintf(fp, "%s", contents);',
    (
        "C",
        "grpc",
    ): 'grpc_channel *channel = grpc_insecure_channel_create("localhost:50051", NULL, NULL);',
    (
        "C",
        "email",
    ): 'curl_easy_setopt(curl, CURLOPT_URL, "smtp://smtp.example.com:587");\ncurl_easy_perform(curl);',
    (
        "C",
        "caching",
    ): 'redisReply *reply = redisCommand(ctx, "SET user:123 %s", user_json);',
    (
        "C",
        "scheduling",
    ): "timer_create(CLOCK_REALTIME, &sev, &timerid);\ntimer_settime(timerid, 0, &its, NULL);",
    (
        "C",
        "ftp_sftp",
    ): 'curl_easy_setopt(curl, CURLOPT_URL, "sftp://example.com/upload/file.csv");',
    # ── C++ ──
    (
        "C++",
        "http_rest",
    ): 'auto response = cpr::Get(cpr::Url{"https://api.example.com/users"});',
    ("C++", "soap"): "soap_call_ns__getUser(soap, endpoint, action, userId, &result);",
    (
        "C++",
        "messaging",
    ): 'producer->produce(RdKafka::Topic::create(producer, "orders-topic"), payload);',
    (
        "C++",
        "socket",
    ): "boost::asio::ip::tcp::acceptor acceptor(io_context, tcp::endpoint(tcp::v4(), 8080));",
    (
        "C++",
        "database",
    ): 'pqxx::result r = txn.exec("SELECT * FROM users WHERE active = true");',
    ("C++", "file_io"): 'std::ofstream out("output.csv");\nout << contents;',
    (
        "C++",
        "grpc",
    ): 'auto channel = grpc::CreateChannel("localhost:50051", grpc::InsecureChannelCredentials());',
    (
        "C++",
        "email",
    ): "vmime::net::smtp::SMTPTransport transport(session);\ntransport->send(message);",
    (
        "C++",
        "caching",
    ): 'reply = (redisReply*)redisCommand(ctx, "SET user:123 %s", user_json);',
    (
        "C++",
        "scheduling",
    ): "boost::asio::steady_timer timer(io_context, boost::asio::chrono::hours(1));",
    (
        "C++",
        "ftp_sftp",
    ): "LIBSSH2_SFTP_HANDLE *handle = libssh2_sftp_open(sftp_session, remote_path, flags, mode);",
    # ── COBOL ──
    (
        "COBOL",
        "http_rest",
    ): '           MOVE "GET" TO WS-HTTP-METHOD.\n           MOVE "https://api.example.com/users" TO WS-URL.\n           CALL "HTTPAPI" USING WS-HTTP-METHOD WS-URL WS-RESPONSE.',
    (
        "COBOL",
        "soap",
    ): "           XML PARSE WS-SOAP-RESPONSE\n               PROCESSING PROCEDURE XML-HANDLER.",
    (
        "COBOL",
        "messaging",
    ): "           CALL 'MQPUT' USING HCONN HOBJ MQMD MQPMO MSGLENGTH MSGBUFFER CC RC.",
    (
        "COBOL",
        "socket",
    ): '           CALL "EZASMI" USING SOC-FUNCTION SOCKADDR ERRNO RETCODE.',
    (
        "COBOL",
        "database",
    ): "           EXEC SQL SELECT CUST_NAME INTO :WS-NAME FROM CUSTOMER WHERE CUST_ID = :WS-ID END-EXEC.",
    (
        "COBOL",
        "file_io",
    ): "           OPEN OUTPUT CUSTOMER-FILE.\n           WRITE CUSTOMER-RECORD.",
    # ── PL/I ──
    (
        "PL/I",
        "http_rest",
    ): "CALL HWTHINIT(REQUEST_HANDLE, HWTH_HANDLETYPE_HTTPREQUEST, RC);",
    ("PL/I", "soap"): "CALL EZASMI('SEND', SOCKET_DESC, SOAP_BUFFER, SOAP_LEN, RC);",
    (
        "PL/I",
        "messaging",
    ): "CALL MQPUT(HCONN, HOBJ, MQMD, MQPMO, MSG_LEN, MSG_BUFFER, CC, RC);",
    (
        "PL/I",
        "socket",
    ): "CALL EZASMI('SOCKET', AF_INET, SOCK_STREAM, 0, SOCKDESC, ERRNO, RETCODE);",
    (
        "PL/I",
        "database",
    ): "EXEC SQL SELECT CUST_NAME INTO :CUST_NAME FROM CUSTOMER WHERE CUST_ID = :CUST_ID;",
    (
        "PL/I",
        "file_io",
    ): "OPEN FILE(CUSTFILE) OUTPUT;\nWRITE FILE(CUSTFILE) FROM(CUST_RECORD);",
    # ── Pascal ──
    (
        "Pascal",
        "http_rest",
    ): "Response := IdHTTP1.Get('https://api.example.com/users');",
    (
        "Pascal",
        "soap",
    ): "HTTPRIO1.URL := 'https://example.com/service?wsdl';\nResult := (HTTPRIO1 as IMyService).GetUser(UserId);",
    ("Pascal", "messaging"): "MQTTClient.Publish('orders/topic', Payload);",
    (
        "Pascal",
        "socket",
    ): "IdTCPServer1.DefaultPort := 8080;\nIdTCPServer1.Active := True;",
    (
        "Pascal",
        "database",
    ): "FDQuery1.SQL.Text := 'SELECT * FROM users WHERE active = 1';\nFDQuery1.Open;",
    (
        "Pascal",
        "file_io",
    ): "AssignFile(F, 'output.csv');\nRewrite(F);\nWriteLn(F, Contents);",
    ("Pascal", "email"): "IdSMTP1.Send(IdMessage1);",
    ("Pascal", "scheduling"): "Timer1.Interval := 3600000;\nTimer1.Enabled := True;",
    ("Pascal", "ftp_sftp"): "IdFTP1.Put(LocalPath, RemotePath);",
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
    # Embed descriptions once
    desc_keys = list(DESCRIPTIONS.keys())
    desc_texts = [DESCRIPTIONS[k] for k in desc_keys]
    print("Embedding descriptions...", file=sys.stderr)
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
    correct = 0
    total = 0
    results_by_lang = {}

    for fi, (lang, expected_type) in enumerate(frag_keys):
        vec_code = frag_embeddings[fi]
        scores = {
            desc_keys[di]: cosine(vec_code, desc_embeddings[di])
            for di in range(len(desc_keys))
        }
        predicted = max(scores, key=scores.get)
        is_correct = predicted == expected_type

        correct += int(is_correct)
        total += 1

        if lang not in results_by_lang:
            results_by_lang[lang] = {"correct": 0, "total": 0, "misses": []}
        results_by_lang[lang]["total"] += 1
        results_by_lang[lang]["correct"] += int(is_correct)
        if not is_correct:
            results_by_lang[lang]["misses"].append(
                (expected_type, predicted, scores[expected_type], scores[predicted])
            )

    # Print results
    print()
    print(f"{'Language':<14} {'Correct':>7} {'Total':>5} {'Accuracy':>8}")
    print("-" * 38)
    for lang in [
        "Java",
        "Python",
        "TypeScript",
        "JavaScript",
        "Go",
        "Rust",
        "C#",
        "C++",
        "C",
        "Kotlin",
        "Scala",
        "PHP",
        "Ruby",
        "COBOL",
        "PL/I",
        "Pascal",
    ]:
        if lang not in results_by_lang:
            continue
        r = results_by_lang[lang]
        pct = r["correct"] / r["total"] * 100
        print(f"{lang:<14} {r['correct']:>7} {r['total']:>5} {pct:>7.1f}%")

    print("-" * 38)
    pct_total = correct / total * 100
    print(f"{'TOTAL':<14} {correct:>7} {total:>5} {pct_total:>7.1f}%")

    # Print misclassifications
    all_misses = [
        (lang, *miss) for lang, r in results_by_lang.items() for miss in r["misses"]
    ]
    if all_misses:
        print(f"\nMisclassifications ({len(all_misses)}):")
        print(
            f"{'Language':<14} {'Expected':<16} {'Predicted':<16} {'Exp Score':>9} {'Pred Score':>10}"
        )
        print("-" * 70)
        for lang, expected, predicted, exp_score, pred_score in all_misses:
            print(
                f"{lang:<14} {expected:<16} {predicted:<16} {exp_score:>9.4f} {pred_score:>10.4f}"
            )


if __name__ == "__main__":
    main()
