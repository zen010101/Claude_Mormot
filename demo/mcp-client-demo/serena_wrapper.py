#!/usr/bin/env python3
"""Wrapper to run serena MCP server with proper stdio handling"""
import sys
import subprocess
import threading

def forward_stderr(proc):
    """Forward stderr to a file for debugging"""
    with open('serena_stderr.log', 'w') as f:
        for line in proc.stderr:
            f.write(line)
            f.flush()

def main():
    proc = subprocess.Popen(
        ['uv', 'run', '--directory', 'D:/Gits/serena',
         'serena', 'start-mcp-server', '--context=claude-code'],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        bufsize=1,  # Line buffered
    )

    # Forward stderr to file in background
    stderr_thread = threading.Thread(target=forward_stderr, args=(proc,), daemon=True)
    stderr_thread.start()

    # Forward stdin -> subprocess stdin
    # Forward subprocess stdout -> stdout
    def forward_stdin():
        try:
            for line in sys.stdin:
                proc.stdin.write(line)
                proc.stdin.flush()
        except:
            pass

    stdin_thread = threading.Thread(target=forward_stdin, daemon=True)
    stdin_thread.start()

    # Forward stdout
    try:
        for line in proc.stdout:
            sys.stdout.write(line)
            sys.stdout.flush()
    except:
        pass

if __name__ == '__main__':
    main()
