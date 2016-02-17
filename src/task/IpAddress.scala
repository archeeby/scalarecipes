package task

import task.IpAddressType._

class IpAddress(r1: Short, r2: Short, r3: Short, r4: Short, addressType: IpAddressType) {
  override def toString = r1 + "." + r2 + "." + r3 + "." + r4 + " " + addressType
}

object IpAddress {
  def apply(r1: Short, r2: Short, r3: Short, r4: Short, addressType: IpAddressType): IpAddress =
    new IpAddress(r1, r2, r3, r4, addressType)
}