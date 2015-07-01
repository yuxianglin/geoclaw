import os
import shutil

def remove(files):
    """Removes one or more files or directories"""
    if isinstance(files,str): #is files a string
        files = [files]
    if not isinstance(files,list):
        "Error"
    for file in files:
        if os.path.isdir(file):
            shutil.rmtree(file)
        elif os.path.isfile(file):
            os.remove(file)
