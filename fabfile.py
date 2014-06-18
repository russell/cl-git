from fabric.api import local, lcd, abort, run, settings, env
from fabric.contrib.project import rsync_project
from fabric.decorators import task
from os.path import expandvars
import os

env.use_ssh_config = True


def clean():
    with lcd("doc"):
        local("rm -rf html/")


@task
def build():
    """build the documentation"""
    clean()
    with lcd("doc"):
        local("sphinx-build -b html -E . html")


@task
def doc_server(port="8000"):
    """Serve documentation from localhost.

    @param port  Port to run server on.
    """
    with lcd("doc/html"):
        local("python -m SimpleHTTPServer %s" % port)


def add(x, y):
    return x + y


def inc_version(major=0, minor=0, patch=0):
    version = local("git describe --tags --abbrev=0", capture=True)
    version = map(int, version.split("."))
    version = map(add, version, [major, minor, patch])
    if minor > 0:
        version[2] = 0
    version = map(str, version)
    return ".".join(version)


@task
def release_minor():
    generate_version(inc_version(patch=1))


@task
def release_major():
    generate_version(inc_version(minor=1))


def generate_version(version):
    filename = "version.lisp-expr"
    with open(filename, "w") as version_file:
        version_file.write('"' + str(version) + '"')
    local("cat " + filename)
    local("git add " + filename)
    message = "Released " + str(version)
    local("git commit -m '%s'" % message)
    local("git tag -a -m '" + message + "' %s" % version)

try:
    execfile(expandvars("$HOME/.projects.py"))

    @task
    def deploy():
        """deploy a prod version"""
        env = os.environ
        env["GOOGLE_ANALYTICS"] = "true"
        build()
        project_env = PROJECTS["cl-git"]
        env.hosts = [project_env["host"]]
        assert project_env["remote_dir"]
        with settings(host_string=project_env["host"]):
            rsync_project(project_env["remote_dir"], "doc/html/", delete=True)
        clean()

except:
    pass
