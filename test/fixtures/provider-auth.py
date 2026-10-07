"""Local OAuth fixture; credentials are deliberately inert test values."""
import http.server
import json
import pathlib
import sys

class Handler(http.server.BaseHTTPRequestHandler):
    def log_message(self, *_args):
        pass

    def do_POST(self):
        self.rfile.read(int(self.headers.get('Content-Length', '0')))
        if self.path.endswith('/usercode'):
            payload = {'device_auth_id': 'fixture-id', 'user_code': 'ABCD-1234'}
        elif self.path.endswith('/deviceauth/token'):
            payload = {'authorization_code': 'fixture-code', 'code_verifier': 'fixture-verifier'}
        else:
            payload = {'access_token': 'fixture-access', 'refresh_token': 'fixture-refresh',
                       'expires_in': 3600, 'id_token': 'x.eyJzdWIiOiJmaXh0dXJlIn0.x'}
        data = json.dumps(payload).encode()
        self.send_response(int(sys.argv[2]))
        self.send_header('Content-Type', 'application/json')
        self.send_header('Content-Length', str(len(data)))
        self.end_headers()
        self.wfile.write(data)

server = http.server.HTTPServer(('127.0.0.1', 0), Handler)
pathlib.Path(sys.argv[1]).write_text(str(server.server_port))
server.serve_forever()
