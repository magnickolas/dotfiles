#!/usr/bin/env python3

# The original version of this script was stolen from https://unix.stackexchange.com/a/607792

from contextlib import contextmanager
import itertools
import os
import pathlib
import re
import ruamel.yaml
import tempfile as tmp


@contextmanager
def tempfile(suffix="", dir=None):
    tf = tmp.NamedTemporaryFile(delete=False, suffix=suffix, dir=dir)
    tf.file.close()
    try:
        yield tf.name
    finally:
        try:
            os.remove(tf.name)
        except OSError as e:
            if e.errno == 2:
                pass
            else:
                raise


@contextmanager
def open_atomic(filepath, *args, **kwargs):
    fsync = kwargs.get("fsync", False)

    with tempfile(dir=os.path.dirname(os.path.abspath(filepath))) as tmppath:
        with open(tmppath, *args, **kwargs) as file:
            try:
                yield file
            finally:
                if fsync:
                    file.flush()
                    os.fsync(file.fileno())
        os.rename(tmppath, filepath)


def main():
    with open(Config.config_file_path, "r") as config_file:
        yaml = ruamel.yaml.round_trip_load(config_file)
        config_file.seek(0)
        config_lines = config_file.readlines()

    cur_cs_anchor = None
    colors_line_idx = -1
    for i, line in enumerate(config_lines):
        match = re.search(Config.cs_line_pattern, line)
        if match is not None:
            cur_cs_anchor = match.group(1)
            colors_line_idx = i
    assert cur_cs_anchor is not None

    colorschemes = yaml["schemes"]
    colorschemes_anchors = list(
        map(lambda cs_name: colorschemes[cs_name].anchor.value, colorschemes)
    )
    cur_cs_index = colorschemes_anchors.index(cur_cs_anchor)
    new_cs_anchor = next(  # Loop through the list of themes cyclically
        itertools.islice(itertools.cycle(colorschemes_anchors), cur_cs_index + 1, None)
    )

    config_lines[colors_line_idx] = Config.cs_line_template.format(new_cs_anchor)
    with open_atomic(Config.config_file_path, "w") as config_file:
        for line in config_lines:
            config_file.write(line)


def get_config_file_path() -> str:
    XDG_CONFIG_HOME = os.getenv("XDG_CONFIG_HOME")
    HOME = os.getenv("HOME")
    CONFIG_FILE_POSSIBLE_PATHS = []
    if XDG_CONFIG_HOME is not None:
        CONFIG_FILE_POSSIBLE_PATHS.append(
            pathlib.Path(XDG_CONFIG_HOME) / "alacritty/alacritty.yml"
        )
        CONFIG_FILE_POSSIBLE_PATHS.append(
            pathlib.Path(XDG_CONFIG_HOME) / "alacritty.yml"
        )
    if HOME is not None:
        CONFIG_FILE_POSSIBLE_PATHS.append(
            pathlib.Path(HOME) / ".config/alacritty/alacritty.yml"
        )
        CONFIG_FILE_POSSIBLE_PATHS.append(pathlib.Path(HOME) / ".alacritty.yml")
    return next(
        itertools.dropwhile(lambda file: not file.exists(), CONFIG_FILE_POSSIBLE_PATHS)
    )


class Config:
    config_file_path = get_config_file_path()
    cs_line_pattern = r"colors: \*(\S+)"
    cs_line_template = "colors: *{}\n"


if __name__ == "__main__":
    main()
