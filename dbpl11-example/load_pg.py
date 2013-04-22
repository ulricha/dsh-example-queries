#!/usr/bin/python

import os
import sys
import psycopg2

if len(sys.argv) != 6:
    sys.exit("usage: load_pg <db> <user> <nr_teams> <from> <to>")

db = sys.argv[1]
user = sys.argv[2]
nr_teams = int(sys.argv[3])
from_exp = int(sys.argv[4])
to_exp = int(sys.argv[5])

conn = psycopg2.connect("dbname=%s user=%s" % (db, user))
cur = conn.cursor()

copy_cmd = "COPY %s FROM '%s' CSV"
teams_tbl = "teams_%d" % nr_teams
create_cmd_teams = "CREATE TABLE %s(id int primary key, name char(11))" % teams_tbl
copy_cmd_teams =  copy_cmd % (teams_tbl, os.getcwd() + "/" + teams_tbl + ".csv")
#cur.execute(create_cmd_teams)
#cur.execute(copy_cmd_teams)
conn.commit()

print "teams"

for i in range(from_exp, to_exp + 1):
    players_tbl = "players_%d_%d" % (nr_teams, 10**i)
    create_cmd_players = "CREATE TABLE %s(id int primary key, team int references %s(id), name char(11) not null, pos char(1), eff int not null)" % (players_tbl, teams_tbl)
    copy_cmd_players = copy_cmd % (players_tbl, os.getcwd() + "/" + players_tbl + ".csv")
    cur.execute(create_cmd_players)
    cur.execute(copy_cmd_players)
    print players_tbl
    conn.commit()

cur.close()
conn.close()


