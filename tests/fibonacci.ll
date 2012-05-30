; ModuleID = 'fibonacci.c'
target datalayout = "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:32:32-n8:16:32-S128"
target triple = "i386-pc-linux-gnu"

define i32 @main(i32 %argc, i8** %argv) nounwind {
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  %3 = alloca i8**, align 4
  %i = alloca i32, align 4
  %n = alloca i32, align 4
  %num1 = alloca i32, align 4
  %num2 = alloca i32, align 4
  %temp = alloca i32, align 4
  store i32 0, i32* %1
  store i32 %argc, i32* %2, align 4
  store i8** %argv, i8*** %3, align 4
  store i32 100, i32* %n, align 4
  store i32 0, i32* %num1, align 4
  store i32 1, i32* %num2, align 4
  store i32 0, i32* %i, align 4
  br label %4

; <label>:4                                       ; preds = %14, %0
  %5 = load i32* %i, align 4
  %6 = load i32* %n, align 4
  %7 = icmp slt i32 %5, %6
  br i1 %7, label %8, label %17

; <label>:8                                       ; preds = %4
  %9 = load i32* %num1, align 4
  %10 = load i32* %num2, align 4
  %11 = add nsw i32 %9, %10
  store i32 %11, i32* %temp, align 4
  %12 = load i32* %num2, align 4
  store i32 %12, i32* %num1, align 4
  %13 = load i32* %temp, align 4
  store i32 %13, i32* %num2, align 4
  br label %14

; <label>:14                                      ; preds = %8
  %15 = load i32* %i, align 4
  %16 = add nsw i32 %15, 1
  store i32 %16, i32* %i, align 4
  br label %4

; <label>:17                                      ; preds = %4
  %18 = load i32* %temp, align 4
  ret i32 %18
}
