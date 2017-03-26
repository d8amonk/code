import re

string = "'I AM NOT YELLING!' she said, but we knew that wasn't true."

print(string)

new = re.split(" ", string, 5)

print(new)

new = re.sub('[.,!,,,\']', '', string)

print(new)