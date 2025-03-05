import os
import requests
from bs4 import BeautifulSoup

# URL of the directory listing
BASE_URL = "http://jatinga.iitg.ac.in/~asahu/cs331/JavaThread-Code/"
DOWNLOAD_DIR = "java_files"

# Create directory if it doesn't exist
os.makedirs(DOWNLOAD_DIR, exist_ok=True)

def get_java_files():
    response = requests.get(BASE_URL)
    if response.status_code != 200:
        print("Failed to fetch the webpage.")
        return []
    
    soup = BeautifulSoup(response.text, "html.parser")
    java_files = []
    
    for link in soup.find_all("a"):
        href = link.get("href")
        if href.endswith(".java"):
            java_files.append(href)
    
    return java_files

def download_files(file_list):
    for file in file_list:
        file_url = BASE_URL + file
        file_path = os.path.join(DOWNLOAD_DIR, file)
        
        response = requests.get(file_url)
        if response.status_code == 200:
            with open(file_path, "wb") as f:
                f.write(response.content)
            print(f"Downloaded: {file}")
        else:
            print(f"Failed to download: {file}")

if __name__ == "__main__":
    java_files = get_java_files()
    if java_files:
        print(f"Found {len(java_files)} Java files. Downloading...")
        download_files(java_files)
    else:
        print("No Java files found.")
