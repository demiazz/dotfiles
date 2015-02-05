#!/usr/bin/python


from __future__ import absolute_import

import random
import string


def generate_password(length):
    symbols = "{0}{1}".format(string.digits, string.letters)

    return ''.join(random.choice(symbols) for i in range(length))


class FilterModule(object):
    """Ansible password helper"""

    def filters(self):
        return { "generate_password": generate_password }
