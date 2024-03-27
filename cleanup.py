import os

def get_hm_links(path: str) -> list[str]:
    links = os.listdir(path)
    link_path = list(map(lambda x: path + x, links))
    link_path.sort(key=lambda x: os.stat(x).st_ctime)

    link_path.remove(path + 'home-manager')
    
    return link_path

def rm_links(links: list[str]) -> None:

    for link in links[:-2]:
        if os.path.exists(link):
            print("Deleting the home-manager-link: {}".format(link))
            os.remove(link)

if __name__ == '__main__':

    path = "/Users/dez/.local/state/nix/profiles/"

    rm_links(get_hm_links(path))
