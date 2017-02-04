class Prize:
    def __init__(self, name, age, year, prize_type):
        self.name = unicodedata.normalize('NFKD', name).encode('ascii', 'ignore')  # umlaut issues

    self.age = age
    self.year = year
    self.prize_type = prize_type

    def __str__(self):
        return self.name + ' won ' + str(self.prize_type) + ' at age ' + str(self.age) + ' in ' + str(self.year)


f = open('nobel_laureates_by_age.html', 'r')
html = BeautifulSoup(f.read())

winners = []
prize_types = set()
nobel_prize_string = "The Nobel Prize in "
for tag in html.find("div", id="nobel-age-info").children:
    # we're looking for a specific div, that doesn't have a class, id, or anything noteworthy
    # so I'm going to count the divs that are in this outerdiv until we hit the one I want
    if tag.name == None:
        next
    elif tag.name == 'h3':
        current_age = int(tag.text.split(" ")[-1])  # update the age
    elif tag.name == 'div':
        name = tag.find("h6").text  # winner's name
        description = tag.find_all("p")[0].find("a").text  # winner's name
        year = int(description.split(' ')[-1])
        prize_type = ' '.join(description.split(' ')[0:-1])
        prize_types.add(prize_type)
        prize = Prize(name, current_age, int(year), prize_type, description)
        winners.append(prize)