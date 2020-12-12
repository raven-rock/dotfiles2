FROM ubuntu:18.04

RUN apt -y update
RUN apt -y install software-properties-common # add-apt-repository

# emacs25
RUN add-apt-repository -y ppa:kelleyk/emacs
RUN apt -y update
RUN apt -y install emacs25

# misc
RUN apt -y install net-tools         # ifconfig
RUN apt -y install iputils-ping      # ping
RUN apt -y install fonts-hack-ttf    # "Hack" - a good open source coding font
RUN apt -y install postgresql-client # psql
RUN apt -y install curl              # curl

# Ripgrep
RUN curl -o /tmp -LO https://github.com/BurntSushi/ripgrep/releases/download/0.8.1/ripgrep_0.8.1_amd64.deb
RUN dpkg -i /tmp/ripgrep_0.8.1_amd64.deb

# gnome-terminal
RUN apt -y install gnome-terminal
