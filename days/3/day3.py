import re
from operator import mul
from functools import reduce # python3 compatibility


with open("./days/3/input.txt") as f:
    string = f.read()
    total = 0
    for match in [x.group() for x in re.finditer('mul\(\d+,\d+\)', string)]:
        total += reduce(mul, map(int, match.replace("mul(", "").replace(")","").split(",")), 1)
    print("Part 1: " + str(total))


with open("./days/3/input.txt") as f:
    string = f.read()
    total = 0
    do = True
    for match in [x.group() for x in re.finditer('mul\(\d+,\d+\)|do\(\)|don\'t\(\)', string)]:
        print(match)
        if match == "do()":
            do = True
        elif match == "don't()":
            do = False
        elif do:        
            total += reduce(mul, map(int, match.replace("mul(", "").replace(")","").split(",")), 1)
        # print(total)
    print("Part 2: " + str(total))