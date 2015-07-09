import remove_file
import os

def take(dirname):
    if os.access(dirname,os.F_OK):
        remove_file.remove(dirname)
    os.mkdir(dirname, 0755)

