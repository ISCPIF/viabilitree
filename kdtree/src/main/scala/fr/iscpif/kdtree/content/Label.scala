package fr.iscpif.kdtree.content

trait Label {
  def label: Boolean
  def relabel(label: Boolean): this.type
}
