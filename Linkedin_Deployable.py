#!/usr/bin/env python
# coding: utf-8

# In[2]:


import streamlit as st
import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LogisticRegression

# Load the dataset
@st.cache_data
def load_data(file_path):
    try:
        return pd.read_csv(file_path)
    except FileNotFoundError:
        st.error("The dataset file was not found. Ensure 'social_media_usage.csv' is in the same directory as the app.")
        return None

# Clean social media usage column
def clean_sm(x):
    return np.where(x == 1, 1, 0)

# Load the data
s = load_data("social_media_usage.csv")
if s is not None:
    # Filter and clean data
    filtered_data = s[(s['income'] <= 9) & (s['educ2'] <= 8) & (s['age'] <= 98)]
    ss = filtered_data[['income', 'educ2', 'par', 'marital', 'sex', 'age']].copy()
    ss.rename(columns={'educ2': 'education', 'par': 'parent', 'marital': 'married', 'sex': 'female'}, inplace=True)
    ss['female'] = np.where(ss['female'] == 2, 1, 0)
    ss['sm_li'] = clean_sm(filtered_data['web1h'])

    # Split data
    y = ss['sm_li']
    X = ss.drop(columns=['sm_li'])
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

    # Train logistic regression model
    log_reg = LogisticRegression(class_weight='balanced', random_state=42)
    log_reg.fit(X_train, y_train)

    # Prediction function
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

    # Streamlit app layout
    st.title("LinkedIn Usage Predictor")
    st.write("Predict whether a person uses LinkedIn and the probability of usage based on personal attributes.")

    # Human-readable options for income and education
    income_labels = {
        1: "Less than $40,000",
        2: "$40,000 - $65,000",
        3: "$65,000 - $85,000",
        4: "$85,000 - $100,000",
        5: "$100,000 - $125,000",
        6: "$125,000 - $150,000",
        7: "$150,000 - $175,000",
        8: "$175,000 - $200,000",
        9: "More than $200,000"
    }

    education_labels = {
        1: "Less than high school",
        2: "High school incomplete",
        3: "High school graduate",
        4: "Some college, no degree",
        5: "Two-year associate degree",
        6: "Four-year bachelor’s degree",
        7: "Some postgraduate education",
        8: "Postgraduate or professional degree"
    }

    # User inputs
    income = st.selectbox("Household Income", options=income_labels.keys(), format_func=lambda x: income_labels[x])
    education = st.selectbox("Education Level", options=education_labels.keys(), format_func=lambda x: education_labels[x])
    parent = st.selectbox("Are you a parent?", options=[0, 1], format_func=lambda x: "Yes" if x == 1 else "No")
    married = st.selectbox("Marital Status", options=[0, 1], format_func=lambda x: "Married" if x == 1 else "Not Married")
    female = st.selectbox("Gender", options=[0, 1], format_func=lambda x: "Female" if x == 1 else "Male")
    age = st.number_input("Age", min_value=1, max_value=98, value=42)

    # Prediction button
    if st.button("Predict LinkedIn Usage"):
        prediction, probability = predict_linkedin_usage(log_reg, income, education, parent, married, female, age)
        result = "LinkedIn User" if prediction == 1 else "Not a LinkedIn User"
        st.write(f"**Prediction:** {result}")
        st.write(f"**Probability of LinkedIn Usage:** {probability * 100:.1f}%")
else:
    st.stop()


