

//As a consequence, an auxiliary constructor can never invoke a superclass
//constructor directly.

//Such a member is accessible from any subclass, but not from other locations.


// Exercise 1: Extend the following BankAccount class to a CheckingAccount class that charges $1
			//for every deposit and withdrawal.


class BankAccount(initialBalance: Double) {
	private var balance = initialBalance
	def currentBalance = balance
	def deposit(amount: Double) = { balance += amount; balance }
	def withdraw(amount: Double) = { balance -= amount; balance }
}

class CheckingAccount(initialBalance:Double) extends BankAccount(initialBalance) {

	override def deposit(amount:Double) = {super.withdraw(1) ; super.deposit(amount) }
	override def withdraw(amount:Double) = {super.withdraw(1) ; super.withdraw(amount) }
}


// Exercise 2: Extend the BankAccount class of the preceding exercise into a class SavingsAccount
				//that earns interest every month (when a method earnMonthlyInterest is called)
				//and has three free deposits or withdrawals every month. Reset the transaction
				//count in the earnMonthlyInterest method.


class SavingsAccount extends BankAccount {

	def earnMonthlyInterest(interestType:Double) = super.deposit(balance*interestType)

}