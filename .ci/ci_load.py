from glob import iglob
from pathlib import Path

from cryptol.connection import connect

not_cryptol = ["README.md", "INSTALL.md"]

if __name__ == '__main__':
    c = connect(reset_server=True)

    top = Path(__file__).parents[1]

    for f in top.glob("**/*"):
        if (f not in not_cryptol) and (f.suffix in [".cry",".md",".ltx",".tex"]):
            print(f"Loading file: {f}...")
            c.load_file(str(f))
