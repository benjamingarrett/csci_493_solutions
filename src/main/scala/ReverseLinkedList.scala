
object ReverseLinkedList {

  def go = {
    val v1 = new ListNode(1, new ListNode(2, new ListNode(3, new ListNode(4, null))))
    println(s"The original list: ${viewList(v1)}")
    val r_imperative = reverseListImperative(v1)
    println(s"The reversed list (imperative): ${viewList(r_imperative)}")
    val v2 = new ListNode(1, new ListNode(2, new ListNode(3, new ListNode(4, null))))
    println(s"The original list again: ${viewList(v2)}")
    val r_functional = reverseListFunctional(v2)
    println(s"The reversed list (functional): ${viewList(r_functional)}")
    println(s"Bad input: ${viewList(reverseListFunctional(null))}")
  }

  def viewList(head: ListNode): String = {
    def view(node: ListNode): String = 
      if (node == null) "(null)" else s"(${node.x})-->>" + view(node.next)
    view(head)
  }

  def reverseListImperative(head: ListNode): ListNode = {
    if (head == null) null
    else if (head.next == null) head
    else {
      var prev: ListNode = null
      var curr: ListNode = head
      var succ: ListNode = head.next
      while(succ != null){
        curr.next = prev
        prev = curr
        curr = succ
        succ = succ.next
      }
      curr.next = prev
      curr
    }
  }

  def reverseListFunctional(head: ListNode): ListNode = {
    def r(prev: ListNode, curr: ListNode, succ: ListNode): ListNode = {
      curr.next = prev
      if (succ == null) curr
      else r(curr, succ, succ.next)
    }
    if (head == null) null
    else if (head.next == null) head
    else r(null, head, head.next)
  }
}
