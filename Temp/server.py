from flask import Flask, render_template
import os
import subprocess


app = Flask (__name__)

@app.route ('/')
def index ():
    return render_template ('index.html')

@app.route ('/my-link/')
def my_link ():
    # Write your python script code here
    subprocess.call([r"mkdir.bat"])
    print ('I got clicked!')
    return "<h1>Results</h1>Results will be displayed here."

if __name__ == '__main__':
    app.run (debug=True)


