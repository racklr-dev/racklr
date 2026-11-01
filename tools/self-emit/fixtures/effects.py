import requests


def fetch(url):
    return requests.get(url)


def read_config(path):
    with open(path) as handle:
        return handle.read()


def main(url, path):
    data = fetch(url)
    return read_config(path)
