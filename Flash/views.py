from django.http import HttpResponse
from django.views.generic import TemplateView
from django.shortcuts import render
import subprocess
import os

def index(request):
    return render(request, "app/index.html")
def new(request):
    return render(request, "app/new.html")
def sample (request):
    # Write your python script code here
    output = "code output"
    return HttpResponse (output)
def run_flash(request):
    if request.POST:
        subprocess.run(['sh', './action.sh'])
        # or subprocess.call(['python', '/path/to/your/file.py'])
        return render(request, './app/index.html', {})
