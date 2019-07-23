package util

class WithUID {

  var uid: WithUID.UID = WithUID.nextUid()

}

object WithUID {

  type UID = Long

  private var uid: UID = 0L

  private def nextUid(): UID = {
    uid += 1L;
    uid
  }

}
