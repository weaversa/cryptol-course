from os.path import relpath
from pathlib import Path
from shutil import copy

from yaml import safe_load

def id(label):
    return (
        label
            .replace(' ', '')
            .replace('-', '')
    )

if __name__ == "__main__":
    CRYPTOL_COURSE_PATH = Path(__file__).resolve().parents[3]
    SCRIPT_PATH = CRYPTOL_COURSE_PATH/'scripts/layercake'
    TEMPLATES_PATH = SCRIPT_PATH/'templates'
    TEMPLATES_TO_RENDER_PATH = TEMPLATES_PATH/'to_render'
    DEPS_YML_PATH = SCRIPT_PATH/'deps.yml'

    DOCS_PATH = CRYPTOL_COURSE_PATH/'docs'
    MISC_PATH = DOCS_PATH/'misc'

    MAX_CHARS = 1000000

    graphical_view_template = (TEMPLATES_PATH/'graphical_view.md').open().read(MAX_CHARS)
    solicitation_template = (TEMPLATES_PATH/'solicitation.md').open().read(MAX_CHARS)
    navigation_template = (TEMPLATES_PATH/'navigation.md').open().read(MAX_CHARS)

    with DEPS_YML_PATH.open() as DEPS_YML:
        deps = safe_load(DEPS_YML)

    label_by_rel_path = {
        ( Path(v).with_suffix('.md') if v else Path("README.md") ): k
        for (k, v) in deps['urls'].items()
    }

    label_by_rel_path[Path("README.md")] = "Cryptol Course"

    for path in TEMPLATES_TO_RENDER_PATH.rglob("*"):

        def md_relpath(target_label):
            r = relpath((TEMPLATES_TO_RENDER_PATH/deps['urls'][ target_label ]).with_suffix('.md'), path.parent)
            return f"{ '' if r.startswith('.') else './' }{r}"

        rpath = path.relative_to(TEMPLATES_TO_RENDER_PATH)
        wpath = DOCS_PATH/rpath

        print(f'{rpath} -> {wpath}')

        wpath.parent.mkdir(parents=True, exist_ok=True)

        if '.md' in rpath.suffixes:
            is_answers = rpath.name.endswith('Answers.md')
            (answers_path, exercise_path) = (None, Path(f"{ str(rpath)[:-len('Answers.md')] }.md")) if is_answers else (Path(f"{str(rpath)[:-len('.md')]}Answers.md"), None)

            if (exercise_path if is_answers else rpath) not in label_by_rel_path:
                print(f'{ (exercise_path if is_answers else rpath) } not in label_by_rel_path')

            with path.open() as R, wpath.open('w') as W:
                template = R.read(MAX_CHARS)

                navlinks = ''

                label = label_by_rel_path.get(exercise_path if is_answers else rpath, '')

                if '{ navigation }' in template:
                    # generate parent/upbranch line

                    parent_label = next((pl for (pl, xs) in deps['paths'].items() if label in xs), None)

                    if parent_label is None:
                        branch_label = next((bl for (_, px) in deps['branches'].items() for (bl, bx) in px.items() if label in bx), None)

                        if branch_label is not None:
                            navlinks += f"|| [ - { branch_label } ]({ md_relpath(branch_label) }) ||\n"
                    else:
                        parent_path = relpath((TEMPLATES_TO_RENDER_PATH/('README.md' if parent_label == 'Cryptol Course' else deps['urls'][ parent_label ])).with_suffix('.md'), path.parent)
                        navlinks += f"|| [ ^ { parent_label } ]({ parent_path }) ||\n"

                    # generate left/here/right line
                    enumerated_path_with_label = list(enumerate(next((xs for (_, xs) in deps['paths'].items() if label in xs), [])))
                    idx = next((n for (n, l) in enumerated_path_with_label if l == label), None)

                    previous_label = enumerated_path_with_label[idx - 1][1] if idx is not None and idx > 0 else None
                    next_label = enumerated_path_with_label[idx + 1][1] if idx is not None and idx + 1 < len(enumerated_path_with_label) else None

                    previous_url = None if previous_label is None else md_relpath(previous_label)
                    next_url = None if next_label is None else md_relpath(next_label)

                    previous_link = '' if previous_label is None else f" [ < { previous_label } ]({ previous_url }) "
                    next_link = '' if next_label is None else f" [ { next_label } > ]({ next_url }) "

                    navlinks += f"|{ previous_link }| **{ label }** |{ next_link }|\n"
                    
                    # generate start line and/or downbranches lines
                    start_label = next((x for x in deps['paths'].get(label, [])), None)
                    branch_labels = next((x for (cl, x) in deps['branches'].items() if label in x), {}).get(label, [])

                    if start_label is not None:
                        navlinks += f"|| [ v { start_label } ]( { md_relpath(start_label) } ) ||\n"
                    
                    if answers_path is not None and (TEMPLATES_TO_RENDER_PATH/answers_path).exists():
                        navlinks += f"|| [ ! {label} (Answers) ](./{ answers_path.name }) ||\n"
                    elif is_answers:
                        navlinks += f"|| [ ? {label} ](./{ exercise_path.name }) ||\n"

                    if branch_labels:
                        navlinks += "||||\n"

                        for branch_label in branch_labels:
                            navlinks += f"|| [ + { branch_label } ]( { md_relpath(branch_label) } ) ||\n"

                svg_url = relpath( TEMPLATES_TO_RENDER_PATH / f"misc/{ id(label) }.gv.svg", path.parent )
                rel_svg_url = f"{ '' if svg_url.startswith('.') else './' }{ svg_url }"

                graphical_view = graphical_view_template.replace('{ url }', rel_svg_url).replace('{ label }', label)
                solicitation = solicitation_template
                navigation = navigation_template.replace('{ navlinks }', navlinks.strip())

                rendering = (
                    template
                        .replace('{ graphical_view }', graphical_view)
                        .replace('{ solicitation }', solicitation)
                        .replace('{ navigation }', navigation)
                )

                W.write(rendering)
        elif path.is_file():
            copy(path, wpath)