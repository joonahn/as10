# Compiler Final Project
이 프로젝트는 Iterative Register Coalescing을 구현하였다.

## Iterative Register Coalescing
Iterative Register Coalescing은 일반적인 Coalescing이 k-colorable 한 그래프를 non-k-colorable 하게 바꾸는 문제를 해결한 Coalescing 방식으로, Coalescing이후 이웃한 high-degree 노드의 갯수가 k개 이하가 될 경우에만 Coalescing을 진행하고 그렇지 않은 경우 Coalescing을 하지 않는 방식이다.

## What I accomplished
Coalescing을 구현하고 동작하는 것을 확인하였다.

## How well it worked
기본 테스트파일 중 하나인 test.fun에 coalescing을 적용하여 보았다.

![Image of Yaktocat](http://141.223.85.85/pos/image/coalesc.PNG)

## Reference
[Lal George and Andrew W. Appel. 1996. Iterated register coalescing. ACM Trans. Program. Lang. Syst. 18, 3 (May 1996), 300-324.](http://dl.acm.org/citation.cfm?id=229546)