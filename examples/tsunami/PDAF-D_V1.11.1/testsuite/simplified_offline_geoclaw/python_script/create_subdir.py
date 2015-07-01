import remove_file
import os

def create_subdir(i):
    if os.access(str(i),os.F_OK):
        remove_file.remove(str(i))
