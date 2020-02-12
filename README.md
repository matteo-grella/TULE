
# Turin University Linguistic Environment (TULE)

TULE is a rule-based dependency parser written in Common Lisp.
It currently supports Italian and English.


Main Author: Leonardo Lesmo


Collaborators: 
- Matteo Grella (2009 - 2012)
- Livio Robaldo


## Requirements

- CLISP
- Python3 (+ujson)

## Usage

Start the socket-based server:

```bash
$ ./serverStarter.sh
```

Test the results using the python script:

```bash
$ python test.py
```

## License

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
