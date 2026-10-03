(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module F = Octra_core.Fee_policy

let expect label condition =
  if not condition then failwith label

let ok = function
  | Ok value -> value
  | Error error -> failwith error

let refused = function
  | Error _ -> true
  | Ok _ -> false

let z = Z.of_int

let policy () =
  F.market ~target:(z 100) ~capacity:(z 200) ~speed:(z 8)
    ~floor:Z.one ~ceiling:(z 1_000_000) |> ok

let test_prices () =
  let market = policy () in
  List.iter (fun (price, used, expected) ->
    expect "price step differs"
      (Z.equal (F.advance market ~price:(z price) ~used:(z used) |> ok)
        (z expected)))
    [1, 0, 1; 1, 100, 1; 1, 101, 2; 80, 0, 70;
     80, 99, 80; 80, 100, 80; 80, 101, 81; 80, 200, 90;
     1_000_000, 200, 1_000_000];
  List.iter (fun (price, used) ->
    expect "invalid market input accepted"
      (refused (F.advance market ~price:(z price) ~used:(z used))))
    [0, 0; -1, 1; 1_000_001, 0; 1, -1; 1, 201];
  for target = 1 to 8 do
    for speed = 1 to 8 do
      let market = F.market ~target:(z target) ~capacity:(z (2 * target))
        ~speed:(z speed) ~floor:(z 3) ~ceiling:(z 48) |> ok in
      for price = 3 to 48 do
        let prior = ref Z.zero in
        for used = 0 to 2 * target do
          let next = F.advance market ~price:(z price) ~used:(z used) |> ok in
          expect "price outside policy" (Z.leq (z 3) next && Z.leq next (z 48));
          expect "price decreases with work" (Z.leq !prior next);
          expect "price direction differs"
            (if used = target then Z.equal next (z price)
             else if used < target then Z.leq next (z price)
             else Z.geq next (z price));
          expect "excess work is free"
            (used <= target || price = 48 || Z.gt next (z price));
          let exact = price * abs (used - target) / (target * speed) in
          let expected = if used > target then price + max 1 exact else price - exact in
          expect "integer price differs" (Z.equal next (z (min 48 (max 3 expected))));
          prior := next
        done
      done
    done
  done

let test_policy () =
  List.iter (fun (target, capacity, speed, floor, ceiling) ->
    expect "invalid fee policy accepted"
      (refused (F.market ~target:(z target) ~capacity:(z capacity)
        ~speed:(z speed) ~floor:(z floor) ~ceiling:(z ceiling))))
    [0, 0, 1, 1, 2; -1, 1, 1, 1, 2; 2, 1, 1, 1, 2;
     1, 2, 0, 1, 2; 1, 2, -1, 1, 2; 1, 2, 1, 0, 2;
     1, 2, 1, -1, 2; 1, 2, 1, 2, 1];
  let fixed = F.market ~target:Z.one ~capacity:Z.one ~speed:Z.one
    ~floor:(z 7) ~ceiling:(z 7) |> ok in
  expect "fixed price changed"
    (Z.equal (F.advance fixed ~price:(z 7) ~used:Z.zero |> ok) (z 7))

let test_payment () =
  for price = 1 to 17 do
    for work = 0 to 31 do
      let held = z (price * work) in
      let offer = F.reserve ~price:(z price) ~work:(z work) ~cap:held |> ok in
      let extra = F.reserve ~price:(z price) ~work:(z work) ~cap:(Z.succ held) |> ok in
      expect "underfunded offer accepted"
        (refused (F.reserve ~price:(z price) ~work:(z work) ~cap:(Z.pred held)));
      expect "excess execution accepted"
        (refused (F.settle offer ~used:(z (work + 1))));
      expect "negative execution accepted" (refused (F.settle offer ~used:Z.minus_one));
      for used = 0 to work do
        let paid = F.settle offer ~used:(z used) |> ok in
        expect "payment differs" (Z.equal paid.charged (z (price * used)));
        expect "payment exceeds reserve" (Z.leq paid.charged held);
        expect "refund is negative" (Z.sign paid.refund >= 0);
        expect "reserve conservation differs" (Z.equal (Z.add paid.charged paid.refund) held);
        expect "unused cap was charged" (F.settle extra ~used:(z used) = Ok paid);
        expect "payment replay differs" (F.settle offer ~used:(z used) = Ok paid)
      done
    done
  done;
  List.iter (fun (price, work, cap) ->
    expect "invalid fee offer accepted"
      (refused (F.reserve ~price:(z price) ~work:(z work) ~cap:(z cap))))
    [0, 1, 1; -1, 1, 1; 1, -1, 1; 1, 1, -1];
  let work = z 10 in
  let low = F.reserve ~price:(z 2) ~work ~cap:(z 100) |> ok in
  let high = F.reserve ~price:(z 7) ~work ~cap:(z 100) |> ok in
  expect "price increased execution allowance"
    (refused (F.settle low ~used:(z 11)) && refused (F.settle high ~used:(z 11)))

let test_large () =
  let large = Z.pow (z 10) 200 in
  let market = F.market ~target:large ~capacity:(Z.mul (z 2) large)
    ~speed:(z 8) ~floor:Z.one ~ceiling:(Z.mul (z 2) large) |> ok in
  let next = F.advance market ~price:large ~used:(Z.mul (z 2) large) |> ok in
  expect "large price differs" (Z.equal next (Z.add large (Z.div large (z 8))));
  let held = Z.mul large large in
  let offer = F.reserve ~price:large ~work:large ~cap:held |> ok in
  let paid = F.settle offer ~used:(Z.pred large) |> ok in
  expect "large refund differs" (Z.equal paid.refund large);
  expect "large conservation differs" (Z.equal (Z.add paid.charged paid.refund) held)

let () =
  test_prices ();
  test_policy ();
  test_payment ();
  test_large ();
  Printf.printf "status = pass test = fee_market\n%!"