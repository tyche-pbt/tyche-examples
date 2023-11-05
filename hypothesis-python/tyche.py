import json
import pprint
import traceback
import coverage
import websocket

global features
features = {}


def features_for_value(value, feature_type):
    dicts = [(fname, f) for ty, feats in features.items()
             for fname, f in feats.items()
             if isinstance(value, ty) and type(f(value)) == feature_type]
    return dict(dicts)


def analyze(f, port):

    def compute_report():
        old_inner = f.hypothesis.inner_test
        ls = []

        def new_inner(*args, **kwargs):
            ls.append(kwargs)
            old_inner(*args, **kwargs)

        f.hypothesis.inner_test = new_inner

        cov = coverage.Coverage(omit=["tyche.py", "hypothesis/*"],
                                check_preimported=True)
        try:
            with cov.collect():
                f()
        except:
            return {
                "properties": {
                    f.__name__: {
                        "outcome": "propertyFailed",
                        "counterExample": {
                            "item": str(ls[-1]),
                            "features": {},
                            "bucketings": {}
                        },
                        "message": traceback.format_exc()
                    }
                }
            }

        cov_report = []
        for file in cov.get_data().measured_files():
            (_, executable, missing, _) = cov.analysis(file)
            if len(executable) == 0:
                continue
            cov_report.append((file, {
                "hitLines": [i for i in executable if i not in missing],
                "missedLines":
                missing,
            }))

        samples = [{
            "item":
            pprint.pformat(list(l.values())[0], width=50, compact=True)
            if len(l) == 1 else pprint.pformat(l, width=50, compact=True),
            "features":
            dict([(f"{k}_{feature}", f(v)) for k, v in l.items()
                  for feature, f in features_for_value(v, int).items()]),
            "bucketings":
            dict([(f"{k}_{bucketing}", f(v)) for k, v in l.items()
                  for bucketing, f in features_for_value(v, str).items()] +
                 [(f"{k}_{bucketing}", str(f(v))) for k, v in l.items()
                  for bucketing, f in features_for_value(v, bool).items()]),
        } for l in ls]

        return {
            "properties": {
                f.__name__: {
                    "outcome": "propertyPassed",
                    "coverage": dict(cov_report),
                    "samples": samples
                }
            }
        }

    report = {"type": "success", "report": compute_report()}
    ws = websocket.create_connection(f"ws://localhost:{port}")
    ws.send(json.dumps(report))


def visualize(port: int = 8181):

    def decorator(f):

        def wrapper(*args, **kwargs):
            analyze(f, port)

        return wrapper

    return decorator
