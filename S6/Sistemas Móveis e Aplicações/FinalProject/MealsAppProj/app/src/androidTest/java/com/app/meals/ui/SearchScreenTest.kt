package com.app.meals.ui

import androidx.compose.ui.test.assertIsDisplayed
import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.test.performTextInput
import androidx.test.ext.junit.runners.AndroidJUnit4
import com.app.meals.ui.screens.SearchScreen
import com.app.meals.ui.viewModels.SearchUiState
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith

@RunWith(AndroidJUnit4::class)
class SearchScreenTest {

    @get:Rule
    val composeTestRule = createComposeRule()

    @Test
    fun `typing in search bar updates query text visually`() {
        var typedText = ""

        composeTestRule.setContent {
            SearchScreen(
                searchUiState = SearchUiState.Idle,
                searchQuery = typedText,
                onQueryChange = { typedText = it },
                onSearchClick = {},
                categories = listOf("Beef", "Vegan"),
                selectedCategory = null,
                onCategoryClick = {},
                onMealClick = {}
            )
        }

        composeTestRule.onNodeWithText("Search for a meal").performTextInput("Pizza")

        assert(typedText == "Pizza")

        composeTestRule.onNodeWithText("Beef").assertIsDisplayed()
        composeTestRule.onNodeWithText("Vegan").assertIsDisplayed()
    }
}