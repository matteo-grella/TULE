#!/usr/bin/env python
# -*- coding: utf-8 -*-

import ujson
import sys
import copy
import socket

class DependencyParser:

    def __init__(self, host = '127.0.0.1', port = 2728, path = '/'):
        self._host = host
        self._port = port
        self._path = path

    def parse(self, text, to_json = True):
        
        # Brutal conversion/check
        if isinstance(text, str):
            text = text
            decoded = False
        else:
            text = text.decode(encoding)
            decoded = True

        text = text.encode('utf-8')
        
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.connect((self._host, self._port))
        sock.send(text+"\n".encode('ascii'))
            
        data = ""
        while True:
            r = sock.recv(4096)
            if not r:
                break
            else:
                data += r.decode('utf-8')
            
        sock.close()

        if to_json:
            return ujson.decode(data)
        else:
            return data.decode('utf-8')

parser = DependencyParser(host='127.0.0.1', port=2728, path ='/')
parsed = parser.parse("Io sono responsabile di quello che dico e non di quello che capisci.")

# Iterate sentences
for sentence in parsed:
    for atom in sentence:
        if atom['surface']['lexcat'] == "TRACE": # e.g. implied subject
            form           = "-"
            lemma          = "-"
        else:
            form           = atom['surface']['form']
            lemma          = atom['morpho'][0]['lemma']

        # basic atom information
        part_of_speech = atom['syn']['edge'][0]['pos']
        atom_id        = atom['syn']['id']
        head           = atom['syn']['head']
        deprel         = atom['syn']['edge'][0]['deprel']

        # print info
        sys.stdout.write("{}\t{}\t{}\t{}\t{}\t{}\n".format(str(atom_id), form, lemma, part_of_speech, str(head), deprel))
    sys.stdout.write("\n")