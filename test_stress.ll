; ModuleID = 'scriptlang_module'
source_filename = "scriptlang_module"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

define i32 @stressTest(i32 %n) {
entry:
  %j = alloca i32, align 4
  %i = alloca i32, align 4
  %total = alloca i32, align 4
  %n1 = alloca i32, align 4
  store i32 %n, ptr %n1, align 4
  store i32 0, ptr %total, align 4
  store i32 0, ptr %i, align 4
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %entry
  %i2 = load i32, ptr %i, align 4
  %n3 = load i32, ptr %n1, align 4
  %lt = icmp slt i32 %i2, %n3
  br i1 %lt, label %for.body, label %for.exit

for.body:                                         ; preds = %for.cond
  store i32 0, ptr %j, align 4
  br label %for.cond4

for.inc:                                          ; preds = %for.exit7
  %i33 = load i32, ptr %i, align 4
  %add34 = add i32 %i33, 1
  store i32 %add34, ptr %i, align 4
  br label %for.cond

for.exit:                                         ; preds = %for.cond
  %total35 = load i32, ptr %total, align 4
  ret i32 %total35

for.cond4:                                        ; preds = %for.inc6, %for.body
  %j8 = load i32, ptr %j, align 4
  %n9 = load i32, ptr %n1, align 4
  %lt10 = icmp slt i32 %j8, %n9
  br i1 %lt10, label %for.body5, label %for.exit7

for.body5:                                        ; preds = %for.cond4
  %i11 = load i32, ptr %i, align 4
  %j12 = load i32, ptr %j, align 4
  %gt = icmp sgt i32 %i11, %j12
  br i1 %gt, label %if.then, label %if.else

for.inc6:                                         ; preds = %if.merge
  %j31 = load i32, ptr %j, align 4
  %add32 = add i32 %j31, 1
  store i32 %add32, ptr %j, align 4
  br label %for.cond4

for.exit7:                                        ; preds = %for.cond4
  br label %for.inc

if.then:                                          ; preds = %for.body5
  %i13 = load i32, ptr %i, align 4
  %mod = srem i32 %i13, 2
  %eq = icmp eq i32 %mod, 0
  br i1 %eq, label %if.then14, label %if.else15

if.else:                                          ; preds = %for.body5
  %i21 = load i32, ptr %i, align 4
  %j22 = load i32, ptr %j, align 4
  %eq23 = icmp eq i32 %i21, %j22
  br i1 %eq23, label %if.then24, label %if.else25

if.merge:                                         ; preds = %if.merge26, %if.merge16
  %total27 = load i32, ptr %total, align 4
  %add28 = add i32 %total27, 1
  %total29 = load i32, ptr %total, align 4
  %sub = sub i32 %total29, 1
  %mul30 = mul i32 %add28, %sub
  store i32 %mul30, ptr %total, align 4
  br label %for.inc6

if.then14:                                        ; preds = %if.then
  %i17 = load i32, ptr %i, align 4
  %j18 = load i32, ptr %j, align 4
  %mul = mul i32 %i17, %j18
  %0 = load i32, ptr %total, align 4
  %1 = add i32 %0, %mul
  store i32 %1, ptr %total, align 4
  br label %if.merge16

if.else15:                                        ; preds = %if.then
  %i19 = load i32, ptr %i, align 4
  %j20 = load i32, ptr %j, align 4
  %add = add i32 %i19, %j20
  %2 = load i32, ptr %total, align 4
  %3 = sub i32 %2, %add
  store i32 %3, ptr %total, align 4
  br label %if.merge16

if.merge16:                                       ; preds = %if.else15, %if.then14
  br label %if.merge

if.then24:                                        ; preds = %if.else
  %4 = load i32, ptr %total, align 4
  %5 = mul i32 %4, 2
  store i32 %5, ptr %total, align 4
  br label %if.merge26

if.else25:                                        ; preds = %if.else
  %6 = load i32, ptr %total, align 4
  %7 = sdiv i32 %6, 2
  store i32 %7, ptr %total, align 4
  br label %if.merge26

if.merge26:                                       ; preds = %if.else25, %if.then24
  br label %if.merge
}
