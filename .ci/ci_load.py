import cryptol
import os

pwd = os.getcwd()

not_cryptol = ["README.md", "INSTALL.md"]

c = cryptol.connect()

for (root, dirs, file) in os.walk(pwd):
    for f in file:
        if '.md' in f:
            if f not in not_cryptol:
                cry_file = os.path.join(root, f)
                print("Loading file: " + cry_file)
                c.load_file(cry_file)
