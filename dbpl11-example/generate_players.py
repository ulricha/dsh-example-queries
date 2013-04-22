#!/usr/bin/python

import math
import random
import csv
import sys

if len(sys.argv) != 3:
	sys.exit("usage: generate_players <nr_teams> <players_by_team>")

nr_teams = int(sys.argv[1])
players_by_team = int(sys.argv[2])
stddev = int(players_by_team * 0.1)


namecounter = 1

def namestring():
    global namecounter
    cstring = str(namecounter)
    clen = len(cstring)
    namecounter = namecounter + 1
    return "N" + (10 - clen) * '0' + cstring

def teamname(n):
    tstring = str(n)
    tlen = len(tstring)
    return "T" + (10 - tlen) * '0' + tstring

def normalint(mu, sigma):
    return int(math.floor(random.normalvariate(mu, sigma)))

def randompos():
    pos = ["C", "G", "F"]
    i = int(round(random.uniform(1,3)))
    return pos[i-1]

def roster(writer, team, nr_players):
    for i in range(nr_players):
        player = {}
        id = namecounter
        name = namestring()
        pos = randompos()
        eff = normalint(15, 3)
        writer.writerow([id, team, name, pos, eff])

playerWriter = csv.writer(open("players_%d_%d.csv" % (nr_teams, players_by_team), "wb"), delimiter = ",")
teamWriter = csv.writer(open("teams_%d.csv" % nr_teams, "wb"), delimiter = ",")

for t in range(nr_teams):
    name = teamname(t)
    #nr_players = normalint(players_by_team, stddev)
    #roster(playerWriter, t, nr_players)
    roster(playerWriter, t, players_by_team)
    teamWriter.writerow([t, name])
