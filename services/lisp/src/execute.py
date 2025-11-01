#!/usr/bin/env python3
import json
import subprocess
import sys
import time
import tempfile
from pathlib import Path

def execute(request: dict) -> dict:
    """Execute code and return results."""
    code = request.get('code', '')
    timeout = request.get('timeout_seconds', 10)
    input_data = request.get('input_data', '')

    # Create a temporary directory for execution
    with tempfile.TemporaryDirectory() as tmpdir:
        work_dir = Path(tmpdir)

        # Write the LISP code to a file
        lisp_file = work_dir / "program.lisp"
        lisp_file.write_text(code)

        # Command to execute LISP code using SBCL
        # We'll use a simple command for now, just evaluating the file
        # In a real implementation, this would involve more robust sandboxing
        # and potentially a custom runner to handle input/output.
        command = ["sbcl", "--script", str(lisp_file)]

        start_time = time.time()
        try:
            process = subprocess.run(
                command,
                capture_output=True,
                text=True,
                timeout=timeout,
                input=input_data # Pass input_data to stdin
            )
            end_time = time.time()

            status = "SUCCESS"
            errors = []
            if process.returncode != 0:
                status = "RUNTIME_ERROR"
                errors.append(process.stderr.strip())

            return {
                'status': status,
                'output': process.stdout.strip(),
                'errors': errors,
                'ir': '', # Not applicable for simple execution
                'assembly': '', # Not applicable for simple execution
                'machine_code': '', # Not applicable for simple execution
                'compile_time_ms': 0, # No explicit compile phase for simple execution
                'codegen_time_ms': 0,
                'assembly_time_ms': 0,
                'execution_time_ms': (end_time - start_time) * 1000
            }
        except subprocess.TimeoutExpired:
            end_time = time.time()
            return {
                'status': 'TIMEOUT',
                'output': 'Execution exceeded timeout',
                'errors': [],
                'ir': '',
                'assembly': '',
                'machine_code': '',
                'compile_time_ms': 0,
                'codegen_time_ms': 0,
                'assembly_time_ms': 0,
                'execution_time_ms': (end_time - start_time) * 1000
            }
        except Exception as e:
            end_time = time.time()
            return {
                'status': 'RUNTIME_ERROR',
                'output': '',
                'errors': [str(e)],
                'ir': '',
                'assembly': '',
                'machine_code': '',
                'compile_time_ms': 0,
                'codegen_time_ms': 0,
                'assembly_time_ms': 0,
                'execution_time_ms': (end_time - start_time) * 1000
            }

if __name__ == '__main__':
    request = json.loads(sys.stdin.read())
    response = execute(request)
    print(json.dumps(response))
