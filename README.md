# R_Studio 연습 코드 모음

### R_studio_prac
R studio 연습 코드

### R_project_review_final 
형태소 분석을 통해 구글 앱스토어 평점 예측

![image](https://user-images.githubusercontent.com/98004520/174715009-2e033cc9-39d8-407a-b1d9-e4811a248727.png)

##### 프로젝트 목표
* 데이터 수집 및 전처리 후 긍정과 부정 분류에 유의미한 데이터를 확인하고 가공한다.
* NaiveBayes 함수를 이용하여 데이터를 학습시키고 정확도를 향상시킨다.
* 이를 시각화로 표시할 수 있다.

![image](https://user-images.githubusercontent.com/98004520/174715089-a7d21e79-68d9-4e87-a67c-a4aa8a2fce44.png)

#### 패키지
* 데이터 수집, 전처리, 모델링 등 적절한 패키지 사용

![image](https://user-images.githubusercontent.com/98004520/174715825-c490e31a-be73-49cd-bf11-869def52e275.png) ![image](https://user-images.githubusercontent.com/98004520/174715875-5554b736-cd31-4da7-9679-9f67e77803a9.png)
    
#### 데이터 수집
* 웹스크래핑시 발생하는 문제들을 확인 후 코드 수정 후 재수집


![image](https://user-images.githubusercontent.com/98004520/174716055-d208a3e2-f432-4728-a927-2516624b401f.png)

#### 데이터 전처리
* 수집한 데이터 전처리 시에 함수를 생성하여 유의미한 결과를 확인 가능
* 데이터 계산 속도의 문제로 글자수가 긴 리뷰와 평점이 1과 5인 리뷰로 모델을 적합시키기로 결정


![image](https://user-images.githubusercontent.com/98004520/174715148-b899a516-9ab8-4e8a-96ab-c0d1df78725e.png)

#### 정확도 도출
* 태깅 항목 추가, TF-IDF 가중치 추가 등 정확도를 향상시킴.

![image](https://user-images.githubusercontent.com/98004520/174715175-537e8d86-911e-4e19-9fc8-80762c2d615a.png)

#### 모델링 확인
* 최종 모델의 과적합이나 과소적합 여부를 확인하기 위해 글자수가 작은 리뷰, 평점이 2와 4인 리뷰로 확인

![image](https://user-images.githubusercontent.com/98004520/174715220-f53b9d76-7e32-4564-aef8-3cd70e97b875.png)

#### 개선점
* 글자수가 긴 리뷰 중심으로 모델을 생성하여 글자수가 작은 리뷰일 수록 정확도가 낮아짐.
* 한글 형태소 위주의 모델 학습으로 영어나 이모티콘 입력 시 정확도가 하락함.
