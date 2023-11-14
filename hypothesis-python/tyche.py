import json
import pprint
import time
import traceback
import websocket

global features
features = {}


def features_for_value(value):
    dicts = [(fname, f) for ty, feats in features.items()
             for fname, f in feats.items() if isinstance(value, ty)]
    return dict(dicts)


def analyze(f, port):

    def compute_report():
        old_inner = f.hypothesis.inner_test
        ls = []

        def new_inner(*args, **kwargs):
            ls.append(kwargs)
            old_inner(*args, **kwargs)

        f.hypothesis.inner_test = new_inner

        run_start = int(time.time())
        try:
            f()
        except:
            return [{
                "type": "test_case",
                "run_start": run_start,
                "property": f.__name__,
                "status": "failed",
                "status_reason": traceback.format_exc(),
                "representation": str(ls[-1]),
                "how_generated": None,
                "features": {},
                "coverage": "no_coverage_info",
                "metadata": {}
            }]

        return [{
            "type":
            "test_case",
            "run_start":
            run_start,
            "property":
            f.__name__,
            "status":
            "passed",
            "status_reason":
            traceback.format_exc(),
            "representation":
            pprint.pformat(list(l.values()), width=50, compact=True),
            "features":
            dict([(f"{k}_{feature}", f(v)) for k, v in l.items()
                  for feature, f in features_for_value(v).items()]),
            "coverage":
            "no_coverage_info",
            "metadata": {}
        } for l in ls]

    report = compute_report()
    ws = websocket.create_connection(f"ws://localhost:{port}")
    ws.send(json.dumps(report))


def visualize(port: int = 8181):

    def decorator(f):

        def wrapper(*args, **kwargs):
            analyze(f, port)

        return wrapper

    return decorator
