describe('app', () => {
  beforeEach(() => {
    cy.visit('/')
  })

  it('starts', () => { })
  
  it('getting data is working', () => {
    cy.get("#app-picker-counter").click();
    cy.get("#app-picker-tickers").should($select => {
        const selectedValues = $select.val()
        expect(selectedValues).to.have.lengthOf(3)
      })
    cy.contains("Get Data").click();
   })
  
})
