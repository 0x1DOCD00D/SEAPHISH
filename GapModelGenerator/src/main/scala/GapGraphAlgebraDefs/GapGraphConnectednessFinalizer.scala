package GapGraphAlgebraDefs

trait GapGraphConnectednessFinalizer:
  //  using this method we create a connected graph where there are no standalone unconnected nodes
  // because it doesn't make much sense to create and use a GAP whose screens cannot be reached from the initial screen
  def linkOrphanedNodesAndInitStates(allNodes: Array[GuiObject]): Unit
  def addInitState(allNodes: Array[GuiObject]): GuiObject