names = ["John", "Martin"]
statuses = ['false','true']
tasks = [6, 5]
projects = [2, 1]

names = ["John", "Martin", "Luke"]
statuses = ['false', 'true', 'false']
projects = [1, 0, 2]
tasks = [2, 0, 1]

def smartAssigning(names, statuses, projects, tasks):

    if len(names) == 1:
        return names[0]
    elif len(names) == 2:
        first = [names[0], statuses[0], tasks[0], projects[0]]
        second = [names[1], statuses[1], tasks[1], projects[1]]

        if first[1] == 'true' and second[1] == 'true':
            print("You're Outta Luck, Chum!")
        elif first[1] == 'true':
            return second[0]
        elif second[1] == 'true':
            return first[0]
        else:
            if first[2] < second[2]:
                return first[0]
            elif first[2] > second[2]:
                return second[0]
            elif first[2] == second[2]:
                if first[3] < second[3]:
                    return first[0]
                elif first[3] > second[3]:
                    return second[0]
                else:
                    print("You're Outta Luck, Chum!")
            else:
                print("You're Outta Luck, Chum!")
    elif len(names) == 3:
        first = [names[0], statuses[0], tasks[0], projects[0]]
        second = [names[1], statuses[1], tasks[1], projects[1]]
        third = [names[2], statuses[2], tasks[2], projects[2]]


        if first[1] == 'true' and second[1] == 'true' and third[1] == 'true':
            print("You're Outta Luck, Chum!")
        elif first[1] == 'true':
            first = second
            second = third
            if first[2] < second[2]:
                return first[0]
            elif first[2] > second[2]:
                return second[0]
            elif first[2] == second[2]:
                if first[3] < second[3]:
                    return first[0]
                elif first[3] > second[3]:
                    return second[0]
                else:
                    print("You're Outta Luck, Chum!")
            else:
                print("You're Outta Luck, Chum!")
        elif second[1] == 'true':
            second = third
            if first[2] < second[2]:
                return first[0]
            elif first[2] > second[2]:
                return second[0]
            elif first[2] == second[2]:
                if first[3] < second[3]:
                    return first[0]
                elif first[3] > second[3]:
                    return second[0]
                else:
                    print("You're Outta Luck, Chum!")
            else:
                print("You're Outta Luck, Chum!")
        else:
            if first[2] < second[2]:
                return first[0]
            elif first[2] > second[2]:
                return second[0]
            elif first[2] == second[2]:
                if first[3] < second[3]:
                    return first[0]
                elif first[3] > second[3]:
                    return second[0]
                else:
                    print("You're Outta Luck, Chum!")
            else:
                print("You're Outta Luck, Chum!")

smartAssigning(names, statuses, projects, tasks)

    # for i in dict['names']:
    #     print(i)
    #
    # for p in dict['projects']:
    #     if p < dict['projects'][p+1]:
    #         p_index = 0
    #         break
    #     else:
    #         p_index = 1
    #         break

# fred's bot
train = [[3,1],
         [6,1],
         [4,-1],
         [5,1]]
def func(train):
    x = float(0)
    l =[]
    for i in train:
        if i[1] > 0:
            x += i[0]
            l.append(i)
    return x/len(l)

#SumofDigits2
def SumofDigits(n):

