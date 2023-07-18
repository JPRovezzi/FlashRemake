from django.http import HttpResponse
from django.views.generic import TemplateView
from django.shortcuts import render

def index(request):
    return render(request, "app/index.html")
def sample (request):
    # Write your python script code here
    output = "code output"
    return HttpResponse (output)