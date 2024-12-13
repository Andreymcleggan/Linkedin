#!/usr/bin/env python
# coding: utf-8

# # Final Project
# ## Andrey McLeggan
# ### 4Dec2024

# ***

# #### Q1

# In[11]:


import pandas as pd
s = pd.read_csv("social_media_usage.csv")
print(s.shape)


# ***

# #### Q2

# In[14]:


import numpy as np
def clean_sm(x):
    return np.where(x == 1, 1, 0)


# In[15]:


toy_data = pd.DataFrame({
    'column1': [1, 0, 2],
    'column2': [1, 3, 1]
})


# In[16]:


toy_data


# In[17]:


cleaned_toy_data = clean_sm(toy_data)


# In[18]:


cleaned_toy_data


# ***

# #### Q3

# In[21]:


import matplotlib.pyplot as plt
filtered_data = s[(s['income'] <= 9) & (s['educ2'] <= 8) & (s['age'] <= 98)]

ss = filtered_data[['income', 'educ2', 'par', 'marital', 'sex', 'age']].copy()
ss.rename(columns={'educ2': 'education', 'par': 'parent', 'marital': 'married', 'sex': 'female'}, inplace=True)

ss['female'] = np.where(ss['female'] == 2, 1, 0)

ss['sm_li'] = clean_sm(filtered_data['web1h'])


# In[22]:


ss = ss.dropna()

for column in ss.columns[:-1]:
    plt.figure(figsize=(6, 4))
    ss.groupby('sm_li')[column].mean().plot(kind='bar')
    plt.title(f"Average {column} by LinkedIn Usage")
    plt.ylabel(f"Average {column}")
    plt.xlabel("LinkedIn Usage (sm_li)")
    plt.show()


# In[23]:


ss.head(10)


# ***

# #### Q4

# In[26]:


y = ss['sm_li']
x = ss.drop(columns=['sm_li'])
print(y.head)


# In[27]:


print(x.head)


# ***

# #### Q5

# In[30]:


from sklearn.model_selection import train_test_split

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)


# #### X_train: Feature set for training (80% of data). Used by the model to learn patterns during training.
# #### X_test: Feature set for testing (20% of data). Used to evaluate the model's performance on unseen data.
# #### y_train: Target vector for training (80% of data). Provides the true labels for supervised learning.
# #### y_test: Target vector for testing (20% of data). Used to measure the model's predictive accuracy.
# 

# ***

# #### Q6

# In[ ]:


from sklearn.linear_model import LogisticRegression

log_reg = LogisticRegression(class_weight='balanced', random_state=42)
log_reg.fit(X_train, y_train)


# ***

# #### Q7

# In[ ]:


from sklearn.metrics import accuracy_score, confusion_matrix, ConfusionMatrixDisplay

y_pred = log_reg.predict(X_test)
accuracy = accuracy_score(y_test, y_pred)
print(f"Model Accuracy: {accuracy:.2f}")


# In[ ]:


conf_matrix = confusion_matrix(y_test, y_pred)
ConfusionMatrixDisplay(conf_matrix, display_labels=["Class 0", "Class 1"]).plot(cmap="Blues")
plt.title("Confusion Matrix")
plt.show()


# #### Actual Negative/Predicted Negative is 99, meaning the model accurately predicted 99 non-linkedin instances
# #### Autual Negative/Predicted Positive is 62, meaning the model falsely predicted 62 non-linkedin that were actually linkedin users
# #### Actual Positive/Predictive Negative is 24, meaning the model predicted 24 instances of non-linkedin users that were infact linkedin users
# #### Actual Positive/Predicted Positive is 67, meaning the model predicted 67 users that were infact users

# ***

# #### Q8

# In[ ]:


conf_matrix = confusion_matrix(y_test, y_pred)

conf_matrix_df = pd.DataFrame(
    conf_matrix,
    index=["Actual Negative", "Actual Positive"],
    columns=["Predicted Negative", "Predicted Positive"]
)
print(conf_matrix_df)


# ***

# #### Q9

# In[ ]:


precision = 67/(67+62)
recall = 67/(67+24)
f1_score = 2* ((precision * recall)/(precision + recall))

print("Precision is", round(precision, 3), 
      "Recall is", round(recall, 3), 
      "and F1 Score is", round(f1_score, 3))


# In[ ]:


from sklearn.metrics import classification_report
report = classification_report(y_test, y_pred, target_names=["Negative", "Positive"])
print(report)


# ***

# #### Q10

# In[ ]:


young_man = pd.DataFrame({'income': [8], 'education': [7], 'parent': [0], 'married': [1], 'female': [1], 'age': [42]})
old_man = pd.DataFrame({'income': [8], 'education': [7], 'parent': [0], 'married': [1], 'female': [1], 'age': [82]})

probability_1 = log_reg.predict_proba(young_man)[:, 1][0]
probability_2 = log_reg.predict_proba(old_man)[:, 1][0]

print("Probability of LinkedIn usage for a 42-year-old:", round(probability_1, 3))
print("Probability of LinkedIn usage for an 82-year-old:", round(probability_2, 3))



# #### The probability decreased by roughly .27 showing that age as a variable has a significant impact on the dependable variable of whether or not the individual is a linkedin user

# In[ ]:


import streamlit as st
import pandas as pd
from sklearn.linear_model import LogisticRegression

def predict_linkedin_usage(model, income, education, parent, married, female, age):
    individual = pd.DataFrame({
        'income': [income],
        'education': [education],
        'parent': [parent],
        'married': [married],
        'female': [female],
        'age': [age]
    })
    probability = model.predict_proba(individual)[:, 1][0]
    prediction = model.predict(individual)[0]  
    return prediction, round(probability, 3)
model = log_reg
st.title("LinkedIn Usage Predictor")
st.write("Predict whether a person uses LinkedIn and the probability of usage based on personal attributes.")

income = st.slider("Income (1 to 9)", min_value=1, max_value=9, value=8)
education = st.slider("Education (1 to 8)", min_value=1, max_value=8, value=7)
parent = st.selectbox("Are you a parent?", options=[0, 1], format_func=lambda x: "Yes" if x == 1 else "No")
married = st.selectbox("Marital Status", options=[0, 1], format_func=lambda x: "Married" if x == 1 else "Not Married")
female = st.selectbox("Gender", options=[0, 1], format_func=lambda x: "Female" if x == 1 else "Male")
age = st.number_input("Age", min_value=1, max_value=98, value=42)

if st.button("Predict LinkedIn Usage"):
    prediction, probability = predict_linkedin_usage(model, income, education, parent, married, female, age)
    result = "LinkedIn User" if prediction == 1 else "Not a LinkedIn User"
    st.write(f"**Prediction:** {result}")
    st.write(f"**Probability of LinkedIn Usage:** {probability * 100:.1f}%")


# In[ ]:




