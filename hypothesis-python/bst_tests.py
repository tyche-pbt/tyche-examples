from bst import *
from hypothesis import assume, strategies as st
import hypothesis
import tyche


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


@tyche.dump_to_file()
@hypothesis.given(trees(), st.integers())
def test_insert_valid(tree, new_value):
    hypothesis.target(tree.size())
    hypothesis.event(
        "is_bst", payload="bst" if tree.is_binary_search_tree() else "not_bst")
    assume(tree.is_binary_search_tree())
    new_tree = tree.insert(new_value)
    assert new_tree.is_binary_search_tree()


@tyche.dump_to_file()
@hypothesis.given(trees(), st.integers())
def test_insert_post(tree, new_value):
    hypothesis.target(tree.size())
    hypothesis.event(
        "is_bst", payload="bst" if tree.is_binary_search_tree() else "not_bst")
    assume(tree.is_binary_search_tree())
    new_tree = tree.insert(new_value)
    assert new_tree.contains(new_value)


# @hypothesis.given(trees(), st.integers())
# def test_delete_valid(tree, new_value):
#     assume(tree.is_binary_search_tree())
#     new_tree = tree.delete(new_value)
#     assert new_tree.is_binary_search_tree()

# @hypothesis.given(trees(), st.integers())
# def test_delete_post(tree, new_value):
#     assume(tree.is_binary_search_tree())
#     new_tree = tree.delete(new_value)
#     assert not new_tree.contains(new_value)
