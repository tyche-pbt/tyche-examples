from bst import *
from hypothesis import assume, given, strategies as st
import hypothesis

import tyche

tyche.features[Tree] = {
    "size": lambda t: t.size(),
    # "depth": lambda t: t.depth(),
    "is_bst": lambda t: "valid" if t.is_binary_search_tree() else "invalid",
    # "size_category": lambda t: "small" if t.size() < 5 else "medium" if t.size() < 10 else "large",
}


@st.composite
def trees(draw, max_depth=3):
    if max_depth == 0:
        return Leaf()
    else:
        if not draw(st.integers(min_value=0, max_value=max_depth)):
            return Leaf()
        return Node(draw(st.integers()), draw(trees(max_depth - 1)),
                    draw(trees(max_depth - 1)))


@st.composite
def bsts(draw, lo=-10, hi=10):
    if lo > hi:
        return Leaf()
    else:
        # Pick a number from a range, if 0 we just return a Leaf
        if not draw(st.integers(min_value=0, max_value=1)):
            return Leaf()

        x = draw(st.integers(min_value=lo, max_value=hi))
        return Node(x, draw(bsts(lo, x - 1)), draw(bsts(x + 1, hi)))


@tyche.visualize()
@hypothesis.given(trees(), st.integers())
def test_insert_valid(tree, new_value):
    assume(tree.is_binary_search_tree())
    new_tree = tree.insert(new_value)
    assert new_tree.is_binary_search_tree()


@tyche.visualize()
@hypothesis.given(trees(), st.integers())
def test_insert_post(tree, new_value):
    assume(tree.is_binary_search_tree())
    new_tree = tree.insert(new_value)
    assert new_tree.contains(new_value)


# @tyche.visualize()
# @hypothesis.given(trees(), st.integers())
# def test_delete_valid(tree, new_value):
#     assume(tree.is_binary_search_tree())
#     new_tree = tree.delete(new_value)
#     assert new_tree.is_binary_search_tree()

# @tyche.visualize()
# @hypothesis.given(trees(), st.integers())
# def test_delete_post(tree, new_value):
#     assume(tree.is_binary_search_tree())
#     new_tree = tree.delete(new_value)
#     assert not new_tree.contains(new_value)
