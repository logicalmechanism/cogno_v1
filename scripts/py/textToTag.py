import json
#

def updateTagDatum(string):
    with open("data/datum/tag_datum.json", "r") as jsonFile:
        data = json.load(jsonFile)
    
    data["fields"][0]["fields"][3]["list"] = [{"bytes":val} for val in convertString(string)]
    print(data)

    with open("data/datum/tag_datum.json", "w") as jsonFile:
        json.dump(data, jsonFile)


def convertString(string):
    s = string.encode('utf-8').hex()
    return reduceString(s, [])


def reduceString(string, output):
    if len(string) <= 64:
        return output + [string]
    return reduceString(string[64:], output + [string[:64]])


if __name__ == "__main__":
    updateTagDatum("This is a very long string that I am  using for testing the auto split function for tagging the chain.")