

from nltk.corpus import wordnet as wn


while True:
    print("=================================")
    keyword = input("Keyword: ")
    if keyword == "q": break
    synlist = wn.synsets(keyword)
    synlist = filter(lambda x: x.pos() == 'n', synlist)
    for i, syn in enumerate(synlist):
        print(f"{i+1}.")
        print(syn.name())
        print(syn.definition())
        print(syn.hypernyms())
