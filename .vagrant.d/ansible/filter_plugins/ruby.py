#!/usr/bin/python


from __future__ import absolute_import
from ansible import errors

import re


def normalize_ruby_version(version):
    pattern = re.compile(r"\Aruby\-")

    if pattern.match(version):
        return pattern.sub('', version)
    else:
        return version

def versions_for_ruby_build(rubies):
    long_format  = re.compile(r"\Aruby\-")
    short_format = re.compile(r"\A\d+\.\d+.*")
    versions     = []

    for ruby in rubies:
        if ruby["version"] == "system":
            continue

        version = { }

        if short_format.match(ruby["version"]):
            version["dir"] = "ruby-%s" % ruby["version"]
        else:
            version["dir"] = ruby["version"]

        if long_format.match(ruby["version"]):
            version["version"] = long_format.sub('', ruby["version"])
        else:
            version["version"] = ruby["version"]

        versions.append(version)

    return versions


class FilterModule(object):
    """Ansible ruby role helper"""

    def filters(self):
        return {
            "versions_for_ruby_build": versions_for_ruby_build,
            "normalize_ruby_version": normalize_ruby_version
        }
