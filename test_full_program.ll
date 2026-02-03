; ModuleID = 'scriptlang_module'
source_filename = "scriptlang_module"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

define i32 @add(i32 %a, i32 %b) {
entry:
  %b2 = alloca i32, align 4
  %a1 = alloca i32, align 4
  store i32 %a, ptr %a1, align 4
  store i32 %b, ptr %b2, align 4
  %a3 = load i32, ptr %a1, align 4
  %b4 = load i32, ptr %b2, align 4
  %add = add i32 %a3, %b4
  ret i32 %add
}

define i32 @factorial(i32 %n) {
entry:
  %i = alloca i32, align 4
  %result = alloca i32, align 4
  %n1 = alloca i32, align 4
  store i32 %n, ptr %n1, align 4
  store i32 1, ptr %result, align 4
  store i32 1, ptr %i, align 4
  br label %while.cond

while.cond:                                       ; preds = %while.body, %entry
  %i2 = load i32, ptr %i, align 4
  %n3 = load i32, ptr %n1, align 4
  %le = icmp sle i32 %i2, %n3
  br i1 %le, label %while.body, label %while.exit

while.body:                                       ; preds = %while.cond
  %result4 = load i32, ptr %result, align 4
  %i5 = load i32, ptr %i, align 4
  %mul = mul i32 %result4, %i5
  store i32 %mul, ptr %result, align 4
  %i6 = load i32, ptr %i, align 4
  %add = add i32 %i6, 1
  store i32 %add, ptr %i, align 4
  br label %while.cond

while.exit:                                       ; preds = %while.cond
  %result7 = load i32, ptr %result, align 4
  ret i32 %result7
}

define i32 @main() {
entry:
  %f = alloca i32, align 4
  %x = alloca i32, align 4
  %call = call i32 @add(i32 5, i32 3)
  store i32 %call, ptr %x, align 4
  %call1 = call i32 @factorial(i32 5)
  store i32 %call1, ptr %f, align 4
  %f2 = load i32, ptr %f, align 4
  ret i32 %f2
}
