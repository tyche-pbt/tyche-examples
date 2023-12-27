from bst import *
from hypothesis import assume, event, given, strategies as st, target


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
        if not draw(st.integers(min_value=0, max_value=3)):
            return Leaf()

        x = draw(st.integers(min_value=lo, max_value=hi))
        return Node(x, draw(bsts(lo, x - 1)), draw(bsts(x + 1, hi)))


@given(bsts(), st.integers())
def test_insert_valid(tree, new_value):
    category = "small" if tree.size() < 3 else "medium" if tree.size(
    ) < 8 else "large"
    event("size_category", payload=category)
    event("size", payload=tree.size())
    event("size / depth",
          payload=tree.size() / tree.depth() if tree.depth() else 0)
    # assume(tree.is_binary_search_tree())
    new_tree = tree.insert(new_value)
    assert new_tree.is_binary_search_tree()


@given(trees(), st.integers())
def test_insert_post(tree, new_value):
    event("lean", payload=tree.lean())
    target(tree.size(), label="size")
    event("depth", payload=tree.depth())
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

if __name__ == "__main__":
    test_insert_post()
    test_insert_valid()
