import http.server, socketserver, time, sys
N=int(sys.argv[2]); GAP=float(sys.argv[3]); SIZE=int(sys.argv[4])
class H(http.server.BaseHTTPRequestHandler):
    protocol_version="HTTP/1.1"
    def do_POST(self):
        n=int(self.headers.get('Content-Length',0)); self.rfile.read(n)
        self.send_response(200); self.send_header("Content-Type","text/event-stream")
        self.send_header("Transfer-Encoding","chunked"); self.end_headers()
        t0=time.time()
        for i in range(N):
            d=("data: {\"i\":%d,\"t\":\"%s\"}\n\n"%(i,"x"*SIZE)).encode()
            self.wfile.write(b"%x\r\n%s\r\n"%(len(d),d)); self.wfile.flush()
            if GAP: time.sleep(GAP)
        for j in range(3):
            d=("data: {\"final\":%d,\"t\":\"%s\"}\n\n"%(j,"y"*33000)).encode(); self.wfile.write(b"%x\r\n%s\r\n"%(len(d),d)); self.wfile.flush()
        d=b"data: [DONE]\n\n"; self.wfile.write(b"%x\r\n%s\r\n0\r\n\r\n"%(len(d),d)); self.wfile.flush()
        sys.stderr.write("server sent all in %.2fs\n"%(time.time()-t0))
    def log_message(self,*a): pass
socketserver.TCPServer.allow_reuse_address=True
class T(socketserver.ThreadingMixIn, socketserver.TCPServer): pass
T(("127.0.0.1",int(sys.argv[1])),H).serve_forever()
