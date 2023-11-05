from hypothesis import given, strategies as st, settings
import tyche

tyche.features[int] = {"value": lambda x: x}


@given(st.integers())
def test_add_zero(x):
    assert x + 0 == x


def is_sorted(xs):
    return all(x <= y for x, y in zip(xs, xs[1:]))


tyche.features[list] = {"length": len, "sorted": is_sorted}


@given(st.lists(st.integers()))
@settings(max_examples=100)
def test_concat_nothing(xs):
    assert xs + [] == xs
