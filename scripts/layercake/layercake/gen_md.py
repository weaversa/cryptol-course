from os.path import relpath
from pathlib import Path
from re import compile as re_compile
from shutil import copy

from jinja2 import Environment, FileSystemLoader
from yaml import add_constructor, add_implicit_resolver, safe_load
from yaml.loader import SafeLoader
from yaml.nodes import Node

PATH_TAG = u'!path'

def id(label):
    return (
        label
            .replace(' ', '')
            .replace('-', '')
    )

def navlinks(label: str, deps: dict, base_path: Path, parent: Path):
    def navlink(icon: str, label: str, path:Path=None, after:bool=False):
        return None if label is None else {
            'icon': icon,
            'label': label,
            'path': relpath((base_path/deps['urls'][ label ]), parent) if path is None else path,
            'after': after,
        }

    # generate parent/upbranch line

    up = navlink(
        *next(
            (('^', pl) for (pl, xs) in deps['paths'].items() if label in xs),
            next(
                (('-', bl) for (_, px) in deps['branches'].items() for (bl, bx) in px.items() if label in bx),
                (None, None)
            )
        )
    )

    # generate left/here/right line
    enumerated_path_with_label = list(enumerate(next((xs for (_, xs) in deps['paths'].items() if label in xs), [])))
    idx = next((n for (n, l) in enumerated_path_with_label if l == label), None)

    previous_label = enumerated_path_with_label[idx - 1][1] if idx is not None and idx > 0 else None
    next_label = enumerated_path_with_label[idx + 1][1] if idx is not None and idx + 1 < len(enumerated_path_with_label) else None

    previous = None if previous_label is None else navlink('<', previous_label)
    next_ = None if next_label is None else navlink('>', next_label, after=True)

    # generate start line, answer/exercise line, and/or downbranches lines
    start = navlink(
        *next(
            (('v', x) for x in deps['paths'].get(label, [])),
            (None, None)
        )
    )

    answer_or_exercise = (
        navlink('?', label, exercise_path)
        if is_answers
        else (
            None
            if answers_path is None
            else navlink('!', label, path=answers_path)
        )
    )

    branch_labels = next((x for x in deps['branches'].values() if label in x), {}).get(label, [])
    branches = [ navlink('+', branch_label ) for branch_label in branch_labels ]

    return {
        'up': up,
        'current': label,
        'previous': previous,
        'next_': next_,
        'start': start,
        'answer_or_exercise': answer_or_exercise,
        'branches': branches,
    }

def relative_path_yaml_constructor(base_path: Path):
    _path = base_path.resolve()

    def constructor(loader: SafeLoader, node: Node):
        p = _path / Path(node.value)

        if not p.is_relative_to(_path):
            raise PermissionError(f"Cannot refer to path { node.value } outside base path for YAML loader")

        if not p.exists():
            raise FileNotFoundError(f"Path {node.value} does not exist (relative to base path for YAML loader)")

        return p.relative_to(_path)

    return constructor

if __name__ == "__main__":
    CRYPTOL_COURSE_PATH = Path(__file__).resolve().parents[3]
    SCRIPT_PATH = CRYPTOL_COURSE_PATH/'scripts/layercake'
    SCRIPT_TEMPLATES_PATH = SCRIPT_PATH/'templates'
    MARKDOWN_TEMPLATES_PATH = CRYPTOL_COURSE_PATH/'templates'
    DEPS_YML_PATH = SCRIPT_PATH/'deps.yml'

    DOCS_PATH = CRYPTOL_COURSE_PATH/'docs'
    MISC_PATH = DOCS_PATH/'misc'

    MAX_CHARS = 1000000

    add_constructor(PATH_TAG, relative_path_yaml_constructor(MARKDOWN_TEMPLATES_PATH), Loader=SafeLoader)
    add_implicit_resolver(PATH_TAG, re_compile('.*\.md'), Loader=SafeLoader)

    with DEPS_YML_PATH.open() as DEPS_YML:
        deps = safe_load(DEPS_YML)

    label_by_rel_path = {
        v: k
        for (k, v) in deps['urls'].items()
    }

    env = Environment(
        loader=FileSystemLoader([SCRIPT_TEMPLATES_PATH, MARKDOWN_TEMPLATES_PATH])
    )

    graphical_view_template = env.get_template('graphical_view.md')
    solicitation_template = env.get_template('solicitation.md')
    navigation_template = env.get_template('navigation.md')

    for path in MARKDOWN_TEMPLATES_PATH.rglob("*"):
        rpath = path.relative_to(MARKDOWN_TEMPLATES_PATH)
        wpath = DOCS_PATH/rpath

        wpath.parent.mkdir(parents=True, exist_ok=True)

        if '.md' in rpath.suffixes:
            stem = rpath.stem
            is_answers = stem.endswith('Answers')
            (answers_path, exercise_path) = (
                (None, rpath.with_stem(stem[:-len('Answers')]))
                if is_answers
                else (
                    a
                    if (a := (rpath.with_stem(f'{stem}Answers'), None))[0].exists()
                    else (None, None)
                )
            ) 

            if (exercise_path if is_answers else rpath) not in label_by_rel_path:
                print(f'{ (exercise_path if is_answers else rpath) } not in label_by_rel_path')

            with wpath.open('w') as W:
                template = env.get_template(str(path.relative_to(MARKDOWN_TEMPLATES_PATH)))

                label = label_by_rel_path.get(exercise_path if is_answers else rpath, '')

                svg_url = relpath( MARKDOWN_TEMPLATES_PATH / f"misc/{ id(label) }.gv.svg", path.parent )
                rel_svg_url = f"{ '' if svg_url.startswith('.') else './' }{ svg_url }"

                graphical_view = graphical_view_template.render({
                    'label': label,
                    'url': rel_svg_url,
                })
                solicitation = solicitation_template.render()
                navigation = navigation_template.render(
                    navlinks(label, deps, MARKDOWN_TEMPLATES_PATH, path.parent)
                )

                rendering = (
                    template.render({
                        'graphical_view': graphical_view,
                        'solicitation': solicitation,
                        'navigation': navigation
                    })
                )

                W.write(rendering)
        elif path.is_file():
            copy(path, wpath)
