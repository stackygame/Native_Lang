# nvcdown.py
import shutil
import os
import sys

repo = {
    "NativeGameDev": "https://raw.githubusercontent.com/youruser/nativelang-modules/main/NativeGameDev.nvmod",
    "NativeGUI": "https://raw.githubusercontent.com/youruser/nativelang-modules/main/NativeGUI.nvmod"
}

def install(name):
    import urllib.request
    if name in repo:
        url = repo[name]
        path = f"modules/{name}.nvmod"
        print(f"Downloading {name}...")
        urllib.request.urlretrieve(url, path)
        print(f"Installed {name} at {path}")
    else:
        print(f"No such package: {name}")

if __name__ == "__main__":
    if len(sys.argv) < 3 or sys.argv[1] != "install":
        print("Usage: nvcdown install <ModuleName>")
    else:
        install(sys.argv[2])
