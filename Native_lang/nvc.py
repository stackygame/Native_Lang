# nvc.py
import sys
from native_lang import interpret

def main():
    if len(sys.argv) < 2:
        print("Usage: nvc <file.nvl>")
        return
    with open(sys.argv[1], 'r') as f:
        interpret(f.read().splitlines())

if __name__ == "__main__":
    main()
