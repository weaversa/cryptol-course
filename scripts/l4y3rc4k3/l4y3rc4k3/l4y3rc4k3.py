from itertools import chain
from more_itertools import pairwise
from pathlib import Path

from graphviz import render
from jinja2 import Environment, FileSystemLoader
from yaml import safe_load

def id(label):
    return (
        label
            .replace(' ', '')
            .replace('-', '')
    )

def url( lab ):
    return (
        lab['_'] if (
            isinstance(lab, dict)
            and ('_' in lab)
        ) else lab
    )

if __name__ == '__main__':
    PROJECT_PATH = Path(__file__).resolve().parents[4]
    CRYPTOL_COURSE_PATH = PROJECT_PATH/'cryptol-course'
    MISC_PATH = CRYPTOL_COURSE_PATH/'misc'
    SCRIPT_PATH = CRYPTOL_COURSE_PATH/'scripts/l4y3rc4k3'
    DEPS_YML_PATH = SCRIPT_PATH/'deps.yml'

    BASE_URL = "https://github.com/weaversa/cryptol-course/tree/L4y3rC4k3"

    with DEPS_YML_PATH.open() as DEPS_YML:
        deps = safe_load(DEPS_YML)

    urls = deps['urls']

    fw_id = max(
        len( id(label) )
        for label in urls
    )

    env = Environment(
        loader=FileSystemLoader(SCRIPT_PATH)
    )

    branch_template = env.get_template('path.gv.jinja')

    for path_label in deps['paths']:
        path_id = id( path_label )

        primary_nodes = list( map(id, deps['paths'][path_label]) )
        primary_edges = list( pairwise( primary_nodes ) )

        branch_nodes = list( map(id, chain(*deps['branches'].get(path_label, {}).values())) )

        context = {
            'path_id': path_id,
            'base_url': BASE_URL,
            'primary_urls': (
                (
                    f'{{0: <{ fw_id }}}'.format(id(label)),
                    f"{ url( lab ) }"
                )
                for (label, lab) in urls.items()
                if id(label) in primary_nodes
            ),
            'branch_urls': (
                (
                    f'{{0: <{ fw_id }}}'.format(id(label)),
                    f"{ url( lab ) }"
                )
                for (label, lab) in urls.items()
                if id(label) in branch_nodes
            ),
            'id_labels': [
                (
                    f'{{0: <{ fw_id }}}'.format(id(label)),
                    label.replace(' ', r'\n') if label in deps['newline_labels'] else label
                )
                for label in urls
                if ' ' in label and id(label) in chain(primary_nodes, branch_nodes)
            ],
            'primary_edges': primary_edges,
            'branch_edges': list(
                ( id(from_label), to_id )
                for (from_label, to_labels) in deps['branches'].get(path_label, {}).items()
                for to_id in map(id, to_labels)
            ),
            'ranks': {
                rank_id: [ id(label) for label in labels ]
                for (rank_id, labels) in chain(
                    deps['ranks'].get(path_label, {}).items(),
                    (
                        (fl, ( next(ptl for (pfl, ptl) in primary_edges if pfl == id(fl)), *tls ))
                        for (fl, tls) in deps['branches'].get(path_label, {}).items()
                    )
                )
            },
        }

        branch_rendering = branch_template.render(**context)

        GV_PATH = MISC_PATH/f'{ path_id }.gv'

        with GV_PATH.open('w') as GV:
            GV.write(branch_rendering)

        render(engine='dot', filepath=GV_PATH, format='png')
        render(engine='dot', filepath=GV_PATH, format='svg')
