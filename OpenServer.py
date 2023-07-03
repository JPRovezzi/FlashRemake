import http.server
import socketserver
import webbrowser

PORT = 8100

Handler = http.server.SimpleHTTPRequestHandler
url = "http://127.0.0.1:8100/Flash.html"

# Open URL in a new tab, if a browser window is already open.
webbrowser.open_new_tab(url)

# Open URL in new window, raising the window if possible.
webbrowser.open_new(url)

with socketserver.TCPServer(("", PORT), Handler) as httpd:
    print("serving at port", PORT)
    httpd.serve_forever()



