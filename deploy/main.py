from __future__ import print_function

import json
import os
import subprocess
import sys
from typing import Dict, List, Optional

class Process:
  def __init__(self, command: List[str], env: Dict[str, str]={}) -> None:
    self.__command = command
    self.__env = dict(os.environ)
    self.__env.update(env)
    self.__spawn()

  def __spawn(self) -> None:
    self.__requests: int = 0
    self.__process = subprocess.Popen(self.__command,
        stdin=subprocess.PIPE, stdout=subprocess.PIPE, env=self.__env)
    print('Spawned {} process'.format(' '.join(self.__command)),
        file=sys.stderr)

  def handler(self, event: dict) -> dict:
    if self.__process.poll() is not None:
      self.__spawn()

    serialized_event: str = json.dumps(event) + '\n'
    self.__process.stdin.write(serialized_event.encode('utf-8'))
    self.__process.stdin.flush()

    response: dict = json.loads(self.__process.stdout.readline())

    self.__requests += 1
    print('Served {} requests'.format(self.__requests), file=sys.stderr)

    return response

hsmain: Optional[Process] = None

def handler(event: dict, context: dict) -> dict:
  global hsmain

  if not hsmain:
    with open('env.json') as f:
      env: Dict[str, str] = json.loads(f.read())

    binary = os.environ['LAMBDA_TASK_ROOT'] + '/hsmain'
    hsmain = Process([binary], env)

  return hsmain.handler(event)
