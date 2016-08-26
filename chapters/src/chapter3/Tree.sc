import chapter3.{Branch, Leaf}
import chapter3.Tree._

val tree = Branch(Branch(Leaf(5), Leaf(6)), Branch(Leaf(2), Leaf(3)))
size(tree)
maximum(tree)
map(tree)(_ + 1)