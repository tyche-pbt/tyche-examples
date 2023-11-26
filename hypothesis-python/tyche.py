import json
import hypothesis
import websocket


def visualize(socket_port=None, output_file=None):

    def decorator(f):
        if not socket_port and not output_file:
            raise Exception("Must specify either socket_port or output_file")

        def wrapper(*args, **kwargs):
            lines = []
            hypothesis.internal.observability.TESTCASE_CALLBACKS.append(  # type: ignore
                lambda test_case: lines.append(test_case))
            f()
            result = "\n".join(json.dumps(line) for line in lines) + "\n"
            if socket_port:
                try:
                    ws = websocket.create_connection(
                        f"ws://localhost:{socket_port}")
                    ws.send(result)
                    ws.close()
                except Exception as e:
                    print(e)
                    pass  # TODO
            if output_file:
                try:
                    with open(output_file, "a") as handle:
                        handle.write(result)
                except Exception as e:
                    print(e)
                    pass  # TODO

        return wrapper

    return decorator
