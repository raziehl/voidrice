#!/usr/bin/python
import os
from subprocess import check_output
from gi.repository import Gtk as gtk, AppIndicator3 as appindicator


output = check_output("xrandr | grep ' connected' | head -n 1 | cut -d ' ' -f1", shell=True).decode().rstrip("\n\r")



def main():
  indicator = appindicator.Indicator.new("customtray", "semi-starred-symbolic", appindicator.IndicatorCategory.APPLICATION_STATUS)
  indicator.set_status(appindicator.IndicatorStatus.ACTIVE)
  indicator.set_menu(menu())
  gtk.main()
def menu():
  menu = gtk.Menu()
  command_one = gtk.MenuItem('100%')
  command_one.connect('activate', high)
  menu.append(command_one)

  
  command_two = gtk.MenuItem('75%')
  command_two.connect('activate', medium_high)
  menu.append(command_two)

  command_three = gtk.MenuItem('50%')
  command_three.connect('activate', medium)
  menu.append(command_three)

  command_four = gtk.MenuItem('25%')
  command_four.connect('activate', low_medium)
  menu.append(command_four)

  command_five = gtk.MenuItem('10%')
  command_five.connect('activate', low)
  menu.append(command_five)


  exittray = gtk.MenuItem('Exit Tray')
  exittray.connect('activate', quit)
  menu.append(exittray)
  
  menu.show_all()
  return menu

def active_command(value):
  redshift_kill()
  os.system("xrandr --output {} --brightness {}".format(output, value))
  redshift_start()

def redshift_kill():
  os.system("pkill -9 redshift")

def redshift_start():
  os.system("nohup redshift >/dev/null 2>&1  &" )

def high(_):
  active_command(1)


def medium_high(_):
  active_command(0.75)

def medium(_):
  active_command(.5)

def low_medium(_):
  active_command(0.25)

def low(_):
  active_command(0.1)

def quit(_):
  gtk.main_quit()
if __name__ == "__main__":
  main()
