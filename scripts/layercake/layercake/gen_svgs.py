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
    CRYPTOL_COURSE_PATH = Path(__file__).resolve().parents[3]
    SCRIPT_PATH = CRYPTOL_COURSE_PATH/'scripts/layercake'
    TEMPLATES_PATH = SCRIPT_PATH/'templates'
    DEPS_YML_PATH = SCRIPT_PATH/'deps.yml'

    DOCS_PATH = CRYPTOL_COURSE_PATH/'docs'
    MISC_PATH = DOCS_PATH/'misc'

    BASE_URL = ".."

    with DEPS_YML_PATH.open() as DEPS_YML:
        deps = safe_load(DEPS_YML)

    urls = {
        id(label): url(rel_url)
        for (label, rel_url) in deps['urls'].items()
    }

    fw_id = max(
        len( _id )
        for _id in urls
    )

    env = Environment(
        loader=FileSystemLoader(TEMPLATES_PATH)
    )

    branch_template = env.get_template('path.gv')

    for path_label in deps['paths']:
        path_id = id( path_label )
        branches = deps['branches'].get(path_label, {})

        primary_nodes = [
            id(node_label)
            for node_label in deps['paths'][path_label]
        ]
        primary_edges = list( pairwise( primary_nodes ) )

        branch_nodes = [
            id(branch_label)
            for branch_label in chain(*branches.values())
        ]
        branch_edges = (
            ( id(from_label), to_id )
            for (from_label, to_labels) in branches.items()
            for to_id in map(id, to_labels)
        )

        (primary_urls, branch_urls) = (
            (
                (
                    f'{{0: <{ fw_id }}}'.format(_id),
                    f"{ urls[_id] }"
                )
                for _id in x
            )
            for x in (primary_nodes, branch_nodes)
        )

        ranks = {
            rank_id: [ id(label) for label in labels ]
            for (rank_id, labels) in chain(
                deps['ranks'].get(path_label, {}).items(),
                (
                    (fl, ( next(ptl for (pfl, ptl) in primary_edges if pfl == id(fl)), *tls ))
                    for (fl, tls) in branches.items()
                )
            )
        }

        id_labels = (
            (
                f'{{0: <{ fw_id }}}'.format(_id),
                label.replace(' ', r'\n') if label in deps['newline_labels'] else label
            )
            for label in deps['urls']
            if ' ' in label and (_id := id(label)) in chain(primary_nodes, branch_nodes)
        )

        context = {
            'path_id': path_id,
            'base_url': BASE_URL,
            'primary_urls': primary_urls,
            'branch_urls': branch_urls,
            'id_labels': id_labels,
            'primary_edges': primary_edges,
            'branch_edges': branch_edges,
            'ranks': ranks,
        }

        branch_rendering = branch_template.render(**context)

        GV_PATH = MISC_PATH/f'{ path_id }.gv'

        GV_PATH.parent.mkdir(parents=True, exist_ok=True)

        with GV_PATH.open('w', newline='') as GV:
            GV.write(branch_rendering)

        [
            render(engine='dot', filepath=GV_PATH, format=format)
            for format in 'svg png'.split()
        ]
